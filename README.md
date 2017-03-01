Overview
========

This is the codebase for Build-it Break-it Fix-it (BIBIFI). 
We have run the contest on RHEL and Ubuntu systems. 
VMs can be provisioned via Docker Swarm or AWS EC2. 

Infrastructure
--------------

__bibifi-webapp__ - Code for the website. 

__bibifi-translator__ - CLI program to interact with the contest's database, insert submissions, etc.

__bibifi-runner__ - Program that waits for submissions, starts VM images, tests submissions, grades participants, etc.

__bibifi-periodic__ - Script that periodically pulls the git repository for each participant. If there are new submissions, `periodic` calls `translator` to insert the submission into the database.

__bibifi-core__ - Library code shared by the webapp, runner, and translator.


Setup
=====

PostgreSQL
----------

BIBIFI uses PostgreSQL as its database backend. 
Here's some SQL to setup your PostgreSQL database. Be sure to set `YOURPASSWORD`!

	CREATE USER bibifi WITH PASSWORD 'YOURPASSWORD';
	CREATE DATABASE bibifi OWNER bibifi ENCODING 'UTF8';
	CONNECT TO bibifi AS bibifi;
	REVOKE ALL ON DATABASE bibifi FROM public;
	SET timezone='UTC';

Here's a SQL example to create a contest. The second command sets the default contest to the one with `url` `"fall2014"`. Customize as needed.

	INSERT INTO "contest" ( url, title, build_start, build_end, break_start, break_end, fix_start, fix_end) VALUES ( 'fall2014', 'Fall 2014 Contest', '2014-08-29 01:00:00', '2014-09-01 01:00:00', '2014-09-05 01:00:00', '2014-09-08 01:00:00', '2014-09-13 01:00:00', '2014-09-15 01:00:00');
	UPDATE "configuration" SET value = 'fall2014' WHERE key = 'default_contest';

You'll need to update the `config/postgresql.yml` configuration file with your database's `username`, `password`, `host`, and `port` information.

Webapp
------

To compile the web application, you will need to install [stack](haskellstack.org). 
Then you can run `stack build` in the `bibifi-webapp` directory:

	cd bibifi-webapp/
	stack build

Contest Problems
================

Contest problems are composed of:

- A problem specification that describes the programs contestants are expected to implement. The specification should include instructions for how to make submissions and how submissions will be graded. 
- A suite of tests that include correctness, performance, and optional tests. 
- A virtual machine image that is used to test submissions. 
- A grading script that is uploaded to the virtual machine to run tests, oracle submissions, and break-it tests.

We have used three different problem specifications in the past. 
One problem is a secure log that tracks and queries the movement of entities through an art gallery.  
Another problem is an ATM server that accepts withdraw, deposit, and account creation requests from a client application. 
The final problem is a queryable database that supports permissions and access control checks. 
If you are interested in using one of these existing projects, [email us](info@builditbreakit.org) and we can probably share the problem materials that we have. 

If you would like to integrate your own problem specifications into the infrastructure, you can use the [Contest Problem API](docs/API.md). 

