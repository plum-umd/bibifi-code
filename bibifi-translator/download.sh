#!/bin/bash

export SESSION=$(echo $2 | sed -e 's/[\/&]/\\&/g')
export CSV=$1
#echo $SESSION

# mkdir resumes;
# ./dist/build/translator/translator retrieve resumes > resumes/resumes.csv;

cat $CSV | sed s/,.*/\ $SESSION/g | awk '{print "curl --cookie \"_SESSION="$2"\" https://www.builditbreakit.org/download/"$1" -J -L -O; mv *.pdf resumes/"$1".pdf; mv *.png resumes/"$1".png; mv *.jpg resumes/"$1".jpg; mv *.jpeg resumes/"$1".jpeg; mv *.docx resumes/"$1".docx; mv *.doc resumes/"$1".doc; mv *.txt resumes/"$1".txt; " }' | /bin/bash
#cat resumes/resumes.csv | sed s/,.*//g | awk '{print "open https://www.builditbreakit.org/download/"$1}'
