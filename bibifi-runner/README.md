README
======

    runner [-t THREAD-COUNT] -p PROBLEM-DIRECTORY [-d DATABASE-CONFIG] [-c CONTEST-URL] [REPO-DIRECTORY]

`runner` is an executable that will run contest tests on EC2 or Docker VM instances. 
It listens for jobs from the database. 
It runs the specified number of threads, and each thread will run one oracle, build, break, or fix submission at a time. 
A team can only run one test at a time. 
`runner` will also run the scorer when necessary. 
The contest url is the unique url identifier for the contest. 
Database settings are loaded from `../config/postgresql.yml`. 
Cloud (Docker/AWS) settings are laoded from `../config/cloud.yml`.
