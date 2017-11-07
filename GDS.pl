#!/usr/bin/perl 

##########################################################################
#
# Script name : Git Deployer Server
# Author : 	Guillaume Seigneuret, Samuel Déal
# Date : 	16/01/12
# Last update : 18/04/13
# Type : 	Deamon
# Version : 	1.3.14
# Description : Receive hook trigger from Git and call the git deployer 
# script
#
# Usage : 	gds [-p pidfile] [-l logfile] [-d]
# 		-d for daemonize
#
##   Copyright (C) 2012-2013 Guillaume Seigneuret (Omega Cube)
#
#   This program is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 3 of the License.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program.  If not, see <http://www.gnu.org/licenses/>
############################################################################

use strict;
use warnings;
use v5.22.1;

no warnings "experimental::lexical_subs";
use feature 'lexical_subs';

use IO::Socket;
use Config::Auto;
use Getopt::Long;
use Proc::Daemon;
use Term::ANSIColor qw(:constants);
use Data::Dumper;
use Pod::Usage;
use Sys::Hostname;
use File::Basename;
use File::Spec;
use POSIX ":sys_wait_h";
use lib qw(.);

use constant {
    DEFAULT_LISTEN => '0.0.0.0',
    DEFAULT_PORT => 32337,
    DEFAULT_SCRIPT => 'scripts/',
    DEFAULT_LOGFILE => '/var/log/gds.log',
    DEFAULT_PIDFILE => '/var/run/gds.pid',
    DEFAULT_SCRIPT => dirname(__FILE__).'/scripts/',
    DEFAULT_DAEMONIZE => 0
};

use constant DEBUG => 1;


my sub log_error;
my sub log_info;
my sub log_debug;
my sub trim;
my sub is_abs;


# Manage child list
my %children_proc;
sub on_child_closed {
    $SIG{CHLD} = \&on_child_closed; # loathe sysV
    local ($!, $?);
    while((my $pid = waitpid(-1, WNOHANG)) > 0) {
        delete $children_proc{$pid};
    }
}
$SIG{CHLD} = \&on_child_closed;

# Manage exit signals
my $exit_asked = 0;
sub on_exit_asked {
    $SIG{INT} = \&on_exit_asked;
    $SIG{TERM} = \&on_exit_asked;
    log_info "Signal received, exiting...";
    $exit_asked = 1;
}
$SIG{INT} = \&on_exit_asked;
$SIG{TERM} = \&on_exit_asked;


my $log_fh = *STDOUT;


sub main {
	$| = 1; # Autoflush
	local $Term::ANSIColor::AUTORESET = 1;

    my %conf = load_conf();
    return pod2usage(-verbose => 1, -exitval => 1) if exists $conf{'error'};
    return pod2usage(-verbose => 1, -exitval => 0) if $conf{'help'};
    return pod2usage(-verbose => 2, -exitval => 0) if $conf{'man'};
   
    die $conf{'script'}." doesn't exists\n" unless -d $conf{'script'}; 

 	my $standard_out;
 	if($conf{'daemonize'}) {
        die "A git deployment server is already running\n" if $conf{'pidfile'} && -e $conf{'pidfile'};
 		Proc::Daemon::Init();
    }

    # Open the log file
    if($conf{'logfile'}) {
        open($log_fh, ">>", $conf{'logfile'}) or die "Unable to open ".$conf{'logfile'};
        $standard_out = select($log_fh);
    }

    if($conf{'daemonize'}) {		
 		# Write the PID file
        my $pid_fh;
 		die "Unable to open ".$conf{'pidfile'}." for writing" unless open($pid_fh, ">".$conf{'pidfile'});
 		print $pid_fh $$;
 		close($pid_fh);
 	}

    my $hostname = hostname;
 	my $server = IO::Socket::INET->new(
 					LocalHost 	=> $conf{'listen'},
 					LocalPort	=> $conf{'port'},
 					Proto		=> 'tcp',			
 					Reuse       => 1,
                    Listen		=> 10 )   # or SOMAXCONN
 		or die "Couldn't be a tcp server on port $conf{'port'} : $@\n";
 
 	log_info "GDS started, waiting for connections...";
    
    while(not $exit_asked) { 	
        while(my $client = $server->accept()) {
            if(my $child_pid = fork()) {  #  We still are in parent
                $children_proc{$child_pid} = 1;
                close($client);
            }
            else {  # We are in the child
                close ($server);  #  Child doesn't need the listner
     
                # Git is a precious child and he can't live without good parents ...
                # So our forked process will wait carefully for its own chidren.
                $SIG{CHLD} = undef;
            
                log_info "Connection from: ", inet_ntoa($client->peeraddr);
                run_client(\%conf, $hostname, $standard_out, $log_fh, $client);
                log_info "Connection closed for ".inet_ntoa($client->peeraddr)." PID $$";
                shutdown($client, 2);
                close($client);
                exit(0);
            }
        }
    }

    foreach my $child_pid (keys %children_proc) {
       log_info "killing $child_pid";
       kill TERM => $child_pid; 
    }

 	log_info "Close Server Called.";
    shutdown($server, 2);
 	close($server);
    
    unlink $conf{'pidfile'} if $conf{'pidfile'} && $conf{'daemonize'};
}

sub run_client {
    my ($conf, $hostname, $standard_out, $log_fh, $client) = @_;

    # Git is a precious child and he can't live without good parents ...
    # So our forked process will wait carefully for its own chidren.
    $SIG{CHLD} = undef;

    # Manage clean exit
    $SIG{INT} = sub { die "\nExiting...\n"; };
    $SIG{TERM} = sub { die "\nExiting...\n"; };

    eval {
        if(DEBUG) {
            print $client "***********************************************\r\n";
            print $client "**               Welcome to GDS              **\r\n";
            print $client "***********************************************\r\n";
        }
        print $client BOLD GREEN "[$hostname]: Connexion OK. please make your request.\r\n";

        while(my $rep = <$client>) {
            $rep = trim($rep); 
            log_info "Asked to interpret: '$rep'";

            if ( $rep =~ /^QUIT/i) {
                print $client BOLD GREEN "[$hostname]: ";				
                print $client "Bye!\n";
                log_info "*** Fin de connexion sur PID $$ ***";
                return;
            } 
            elsif($rep =~ /Project: .*\/([\w\-\.]+)\.git Branch: ([\w\-]+)/ 
                  or $rep =~ /Project: ([\w\-\.]+)\.git Branch: ([\w\-]+)/) {
                # Send the STDout to the client.
                $standard_out = select($client);

                if(DEBUG) {
                    print "Recognized Project : $1\r\n";
                    print "Recognized Branch : $2\r\n";
                }
                print BOLD GREEN "[$hostname]: ";
                print BOLD WHITE "$1/$2\r\n";
                my $_PROJECT = $1;
                my $_BRANCH = $2;

                my $script_cmd = undef;
                if(defined($conf->{'exec'})) {
                    $script_cmd = $conf->{'exec'};
                    $script_cmd =~ s/\$\{SCRIPT_FOLDER\}/$conf->{'script'}/;
                    $script_cmd =~ s/\$\{PROJECT\}/$_PROJECT/;
                    $script_cmd =~ s/\$\{BRANCH\}/$_BRANCH/;
                    log_info $script_cmd;
                }
                else {
                    my $script_file = get_script($conf->{'script'}, $_PROJECT, $_BRANCH);

                    unless(defined($script_file) && $script_file && -e $script_file) {
                        print RED "No git deployment script found for $_PROJECT/$_BRANCH :(\r\n";
                        print RESET "Please check your config file and your script folder.\r\n";

                        # restore the stdout
                        select($standard_out);
                        return;
                    }

                    $script_cmd = "'$script_file' '$_PROJECT' '$_BRANCH'";
                }
        
                print BOLD GREEN "[$hostname]: ";
                print BOLD WHITE "Launching git deployment script...\r\n";
                eval {
                    $! = undef;
                    $@ = undef;
                    my $child_pid = fork();
                    die("Failed to fork\n") unless defined($child_pid);
                    if($child_pid == 0) {
                        select($client);
                        open(STDERR, ">&", $client);
                        open(STDOUT, ">&", $client);
                        my $config = undef;
                        eval {
                            $config = Config::Auto::parse();
                            1;
                        };
                        if(defined($config) && defined($config->{env})) {
                            foreach my $key (keys %{$config->{env}}) {
                                $ENV{$key} = $config->{env}->{$key};
                            }
                        }
                        exec $script_cmd;
                    }
                    else {
                        waitpid $child_pid, 0;
                    }
                    my $error = $! || ${^CHILD_ERROR_NATIVE}; 
                    die "Unable to run $script_cmd: $error\n" if $? == -1 and $error;
                    die "Unable to run $script_cmd\n" if $? == -1;
                    die "child died with signal ".($? & 127).", with coredump\n" if $? & 127 && $? & 128;
                    die "child died with signal ".($? & 127).", without coredump\n" if $? & 127;
                    die "error running $script_cmd: $error\n" if $? != 0 and $error;
                    die "error running $script_cmd\n" if $?;
                    print BOLD GREEN "[$hostname]: ";
                    print BOLD WHITE "Deployement succeed\r\n";
                    1;
                } 
                or do {
                    my $error = $@;
                    log_error "Error: $error";
                    print BOLD RED "Error: ";
                    print RED "$error";                            
                    print RESET "\r\n";
                };
                
                # restore the stdout
                select($standard_out);
                return;
            }
            else {
                print $client BOLD GREEN "[$hostname]:";
                print $client RESET RED "Query malformed.";
                print $client RESET "\r\n";
                log_error "Error: Query malformed: '$rep'.";
                return;
            }
        }
    1;
    };
}

sub trim {
    my @out = grep { defined($_) } @_;
    for(@out) {
        s/^\s+//;
        s/\s+$//;
    }
    return wantarray ? @out : $out[0];
}

sub is_abs {
    my ($filepath) = @_;

    if($^O =~ /Win/ ){  # Windows
        return $filepath =~ /^[A-Z]:\\/;  
    }
    else { # we suppose *nix oses
        return $filepath =~ /^\//;
    }
}

sub load_conf_param {
    my ($config, $param, $default) = @_;

    eval {
        return $default unless defined($config);
        return $default unless defined($config->{"gds"}->{$param});
        return trim($config->{"gds"}->{$param});
        1;
    } or do {
        return $default;
    };
}

sub load_conf {
    my %params = ();
   
    my $config = undef;
    eval { 
        $config = Config::Auto::parse();
        1;
    };
    $params{'listen'} = load_conf_param($config, "listen", DEFAULT_LISTEN);
    $params{'port'} = load_conf_param($config, "port", DEFAULT_PORT);
    $params{'script'} = load_conf_param($config, "script", DEFAULT_SCRIPT);

    $params{'daemonize'} = DEFAULT_DAEMONIZE;
      
    $params{'logfile'} = 0;
    $params{'pidfile'} = 0;

    $params{'help'} = 0;
    $params{'man'} = 0;
    $params{'exec'} = undef;
    $params{'exec'} = $config->{'gds'}->{'exec'} if defined($config->{'gds'}->{'exec'});

    GetOptions(
        'listen|m=s'      => \$params{'listen'},
        'q|port=i'        => \$params{'port'},
        's|script=s'      => \$params{'script'},
        'd|daemonize!'    => \$params{'daemonize'},
        'l|log=s'         => \$params{'logfile'},
        'p|pidfile=s'     => \$params{'pidfile'},
        'm|man!'          => \$params{'man'},
        'h|?|help!'       => \$params{'help'},
    ) or return ('error' => 1);

    if($params{'deamonize'}) {
        $params{'logfile'} = load_conf_param($config, "logfile", DEFAULT_LOGFILE) unless $params{'logfile'};
        $params{'pidfile'} = load_conf_param($config, "pidfile", DEFAULT_PIDFILE) unless $params{'pidfile'};
    }

    $params{'script'} = File::Spec->rel2abs($params{'script'}) unless is_abs($params{'script'});

    $params{'script'} =~ s/\/*$//;  # remove trailing / at the end
    return %params;
}

sub get_script {
    my ($script_folder, $project, $branch) = @_;

    return $script_folder.'/'.$project."_".$branch if -e $script_folder.'/'.$project."_".$branch;
    return $script_folder.'/'.$project if -e $script_folder.'/'.$project;
    return $script_folder.'/default' if -e $script_folder.'/default';

    return undef;
}

sub log_info {
    printf $log_fh "[%12s] INFO: ", time;
    print $log_fh join('', grep { defined($_) } @_);
    print $log_fh "\n";
}

sub log_error {
    printf $log_fh "[%12s] ERROR: ", time;
    print $log_fh join('', grep { defined($_) } @_);
    print $log_fh "\n";
}

sub log_debug {
    if(DEBUG) {
        printf $log_fh "[%12s] DEBUG: ", time;
        print $log_fh join('', grep { defined($_) } @_);
        print $log_fh "\n";
    }
}

main();

__END__

=head1 NAME

GDS.pl - Git Deployer Server

=head1 SYNOPSIS

GDS.pl [options] 

 Options:
   --help, -h, ?        Brief help message
   --man, -m            Full documentation
   --daemonize, -d      Launch GDS as a daemon
   --listen, -m IP      Which ip do you listen (default 0.0.0.0)
   --port, -q PORT      The listened port (default 32337)
   --script, -s FOLDER  The script to look when a git update is detected
   --log, -l FILE       The output log file
   --pid, -p FILE       The pid file (valid only with a deamonized call)

=head1 OPTIONS
=back

=over 1
=item B<--help>
Print a brief help message and exits.

=over 1
=item B<--man>
Prints the manual page and exits.

=over 1
=item B<--daemonize>
The application will double fork and close the default streams.
It's not enabled by default

=over 1
=item B<--port>
The port the application will listening to incomming commands

=over 1
=item B<--script>
The script folder where the scripts to deploy are located

=over 1
=item B<--log>
The log file

=over 1
=item B<--pid>
The pid file

=back

=head1 DESCRIPTION
B<This program> will read the given input file(s) and do something
useful with the contents thereof.

=cut

