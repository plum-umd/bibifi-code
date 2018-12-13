Overview
========

This is the codebase for [Build-it Break-it Fix-it (BIBIFI)](http://builditbreakit.org/). 
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

	stack exec -- bibifi Production --port YOURPORT

Runner
------

	cd bibifi-runner/
	sudo yum install libssh2-devel
	stack build

Contest Problems
================

Contest problems are composed of:

- A problem specification that describes the programs contestants are expected to implement. The specification should include instructions for how to make submissions and how submissions will be graded. 
- An oracle program that demonstrates how the problem specification should be implemented. Throughout the contest, participants can query the oracle's behavior against various inputs. The oracle is also useful for automatically grading break-it submissions. 
- A suite of tests that include correctness, performance, and optional tests. 
- A virtual machine image that is used to test submissions. 
- A grading script that can run on the virtual machine to run tests, oracle submissions, and break-it tests.

We have used three different problem specifications in the past.
One problem is a secure log that tracks and queries the movement of entities through an art gallery.
Another problem is an ATM server that accepts withdraw, deposit, and account creation requests from a client application.
The final problem is a queryable database that supports permissions and access control checks.
If you are interested in using one of these existing projects, [email us](mailto:info@builditbreakit.org) and we can probably share the problem materials that we have.

If you would like to integrate your own problem specifications into the infrastructure, you can use the [Contest Problem API](docs/API.md). 

Running Contests
================

To run a contest, you will need an administrator account on the contest website. 
Visit `/register` to create an account. 
The first account created on the website will automatically be made an administrator.
You can make other users administrators by visiting the `/admin/users` page.

Contest Setup
-------------

Before setting up your contest, make sure all the resources for your contest problem is ready. 

To create a contest, visit `/admin/contests/create` and fill out the form. 
"Contest Name" is the title of your contest, and "Contest URL" is a unique text identifier for your contest that is used in URLs. 

You can create multiple contests via this form on the website. 
There is a notion of a "default contest" which the website gives special priority to. 
For example, links on the homepage will automatically link to the default contest's announcements and scoreboard pages. 
When you create a new contest, it automatically becomes the default. 
You can change the default through the `/admin/contests` page.

You need to insert build-it tests into the website's database. 
You can add correctness, performance, and optional tests by navigating through the `/admin/contests` page. 
Be careful not to make the test scripts in the database too large. 
This could cause the `runner` to crash from running out of memory. 

Before running your contest live, I recommend creating a test contest where you test the oracle through the website and the functionality of each round. 


Build-It
--------

To prepare for build-it, you should create a directory on the filesystem for your problem. 
Typically this contains the oracle executable, but its structure might vary depending on your problem. 

You should also create a backend directory. 
This directory will contain clones of the repos used for contest submissions (in the `repos` subdirectory). 
Logs and other contest relevant files will placed here as well. 

When the build-it round begins, you should start the periodic and runner programs. 
We typically leave them running inside a `screen` so that they keep running during the entire round. 
Make sure that the linux user running `periodic` and `runner` has permission to clone from your git account via ssh. 

Warning: The periodic script currently can sometimes be finicky. You may need to restart it if it gets stuck. 

Break-it
--------

To prepare for break-it, you need to zip up the final build-it submissions for passing teams and place them in the `round2` subdirectory of the backend directory. 
You can zip up build submissions by running the following in the `repos` directory:

    for D in *; do zip -r $D.zip $D -x *.git*; done;
		mv *.zip ../round2/

This process might vary depending on your problem. 

Once break-it starts, you will again need to run `periodic` and `runner` for the duration of the round. 

When the round ends, you may need to manually judge some breaks. 
To make a user a judge, select your contest on the `/admin/contests` page and go to the "Add judge" page. 
Once you have selected your judges, you need to assign judgements to your judges. 
You can do this manually, or you can distribute the judgements evenly by going to the "View judgements" page and clicking "Distribute judgements". 
Judges can see their assigned judgments on the `/judges` page.

Fix-it
------

Running fix-it is similar. 
Make sure `periodic` and `runner` are running during the round. 

Fixes are judged in a manner similar to breaks. 

