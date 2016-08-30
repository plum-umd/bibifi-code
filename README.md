TODO...
====


Setup
-----

Here's some SQL to setup your PostgreSQL database. Be sure to set `YOURPASSWORD`!

	CREATE USER bibifi WITH PASSWORD 'YOURPASSWORD';
	CREATE DATABASE bibifi OWNER bibifi ENCODING 'UTF8';
	CONNECT TO bibifi AS bibifi;
	REVOKE ALL ON DATABASE bibifi FROM public;
	SET timezone='UTC';

Here's a SQL example to create a contest. The second command sets the default contest to the one with `url` `"fall2014"`. Customize as needed.

	INSERT INTO "contest" ( url, title, build_start, build_end, break_start, break_end, fix_start, fix_end) VALUES ( 'fall2014', 'Fall 2014 Contest', '2014-08-29 01:00:00', '2014-09-01 01:00:00', '2014-09-05 01:00:00', '2014-09-08 01:00:00', '2014-09-13 01:00:00', '2014-09-15 01:00:00');
	UPDATE "configuration" SET value = 'fall2014' WHERE key = 'default_contest';

