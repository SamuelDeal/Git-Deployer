#!/usr/bin/env sh

# Script argument
PROJECT="$1"
BRANCH="$2"

# Project Specific 
DEST_FOLDER=/var/www/dashboard
REPO_PATH="/var/lib/git_repos/my_project"

echo "cloninig repo"
git "--git-dir=$REPO_PATH" fetch

echo "Deplotying source files"
git "--git-dir=$REPO_PATH" "--work-tree=${DEST_FOLDER}" checkout -f "origin/$BRANCH"

# Do whatever you want, changing ownership for example
# ...

