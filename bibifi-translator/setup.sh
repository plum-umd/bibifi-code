#!/bin/bash
#
# Sets up the translator by copying over relevant files from webapp. 
# First argument MUST be the src directory of the webapp (ex: ../webapp/src/).

dir=$1

cp $dir/config/models config/models
cp $dir/config/postgresql.yml config/postgresql.yml
cp $dir/PostDependencyType.hs PostDependencyType.hs
cp $dir/BuildSubmissions.hs tmpppppp && sed 's/import Import/import Common\nimport Database.Persist\nimport Yesod.Core/' tmpppppp > BuildSubmissions.hs && rm tmpppppp
