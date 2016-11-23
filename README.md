Overview
========

This is the codebase for Build-it Break-it Fix-it (BIBIFI). 
We have run the contest on RHEL and Ubuntu. 

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


