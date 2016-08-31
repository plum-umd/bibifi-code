README
======

    runner [-t THREAD-COUNT] [-o ORACLE-DIRECTORY] [-c CONTEST-URL] [REPO-DIRECTORY]

`runner` is an executable that will run contest tests on EC2. 
It listens for jobs from the database. 
It runs the specified number of threads, and each thread will run one test at a time. 
A team can only run one test at a time. 
`runner` will also run the scorer when necessary. 
The contest url is the unique url identifier for the contest. 
Database settings are loaded from `/fs/mc2-application/config/postgresql.yml`. 
AWS settings are laoded from `/fs/mc2-application/config/aws.yml`.
