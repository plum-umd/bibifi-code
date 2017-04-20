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

	sudo postgresql-setup initdb
	sudo chkconfig postgresql on
	sudo service postgresql start

	sudo -u postgres -i
	psql


BIBIFI uses PostgreSQL as its database backend. 
Here's some SQL to setup your PostgreSQL database. Be sure to set `YOURPASSWORD`!

	CREATE USER bibifi WITH PASSWORD 'YOURPASSWORD';
	CREATE DATABASE bibifi OWNER bibifi ENCODING 'UTF8';

To enable password logins edit the following:

	sudo vim /var/lib/pgsql/data/pg_hba.conf

Change
	local  all      all          peer
To
	local  all      all          md5 
# TODO: other foreign connections

Finish configuring the database:

	sudo service postgresql restart
	psql -U bibifi

	\c bibifi bibifi
	# CONNECT TO bibifi AS bibifi;
	REVOKE ALL ON DATABASE bibifi FROM public;
	SET timezone='UTC';

You'll need to update the `config/postgresql.yml` configuration file with your database's `username`, `password`, `host`, and `port` information.

Core
----

`bibifi-core` is library code shared by the webapp, runner, and translator.
To compile the library, you will need to install [stack](https://haskellstack.org). 
Then you can run `stack build` in the `bibifi-core` directory:

	cd bibifi-core/
	stack build

Webapp
------

TODO `config/settings.yml`

Once `bibifi-core` is built, you can compile the web application by running `stack build` in the `bibifi-webapp` directory:

	cd bibifi-webapp/
	stack build

Run executable:

	stack exec -- bibifi-webapp Production --port YOURPORT

To create a contest, visit `/admin/contests/create` and fill out the form. 

Runner
------

	sudo yum install libssh2-devel
	stack build
