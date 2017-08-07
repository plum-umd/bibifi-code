#!/bin/sh

for D in ./*/
do
   (cd "$D" && git filter-branch --force --env-filter '
   CORRECT_NAME=`echo $GIT_COMMITTER_NAME | sha256sum`
   CORRECT_EMAIL=`echo $GIT_COMMITTER_EMAIL | sha256sum`
   export GIT_COMMITTER_NAME="$CORRECT_NAME"
   export GIT_COMMITTER_EMAIL="$CORRECT_EMAIL"
   export GIT_AUTHOR_NAME="$CORRECT_NAME"
   export GIT_AUTHOR_EMAIL="$CORRECT_EMAIL"
   ' --tag-name-filter cat -- --branches --tags
   );
done


