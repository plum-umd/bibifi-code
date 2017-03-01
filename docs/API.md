Contest Problem API
===================

You can integrate your own problem specifications into the BIBIFI infrastructure by using this Contest Problem API. 

... TODO...


VM Setup
--------

users (ubuntu w/ sudo, no password; builder w/ no sudo), timeout executable

JP: Move to README?


Oracle Submissions
------------------

- User input from web form is JSON
- Starts a VM instance.
- Sends all files in the problem directory.
- TODO: Sends tarred? and uncompresses submission from ... XXX
- Runs oracle (/problem/grader) as user "ubuntu" where the first argument is a filepath to a JSON file. The JSON file has the "type" key set to the value "oracle" while the "input" key contains whatever JSON the user submitted on the website. Here's an example:

{
	"type": "oracle",
	"input": ["whatever","JSON","the user","submitted in the web form"]
}

- Parses resulting JSON stdout

TODO: result formatting... XXX


Build Submissions
-----------------

- Tests are test scripts database (JSON format)
- Start VM instance
- Uploads and untars build submissions to /home/builder/submission
- Compiles build submission with `sudo -i -u builder make -B -C /home/builder/submission/build`
- Runs each build test
	- Runs grader (/problem/grader) as user "ubuntu" where the first argument is a filepath to a JSON file. The JSON file has the "type" key set to the value "build" and key "input" is the JSON test from the database (inserted via the admin interface of the website). A "port" key is also provided for the case when a test needs to start a service on a unique port.

{
	"type": "build",
	"input": {"some":"testcase","expected":"output?"},
	"port": 6300
}

	- A timeout will be recorded if not all required tests finish within the allotted time


- Parses resulting JSON stdout

TODO: result formatting... XXX

Break Submissions
-----------------

- Starts a VM instance.
- Sends all files in the problem directory.
- Sends and unzips target's zip submission from (...)
- Sends break submission folder to "/break/" ...

TODO...

Fix Submissions
---------------

TODO...
sudo -i -u builder make -B -C /home/builder/submission/fix/code/build
TODO...

- Runs all required core and performance tests. 
- Runs all break tests that were automatically graded (status is BreakTested). Judges need to later verify that the rest of the break tests are fixed.



