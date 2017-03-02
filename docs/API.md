Contest Problem API
===================

You can integrate your own problem specifications into the BIBIFI infrastructure by using this Contest Problem API. 
Contest problems require a Docker Swarm or AWS EC2 virtual machine image and a grading script that can run oracle submissions, build-it submissions, and break-it submissions. 
The grading script must accept JSON input and output as described in this document. 
You can find more general information about contest problems and infrastructure setup in the [README](../README.md). 


Virtual Machine Setup
---------------------

Virtual machine images should be runnable through AWS EC2 or Docker Swarm. 
The images should have a linux OS (we've used Ubuntu). 
SSH should be configured so that users can log in with ssh keys. 
The user "ubuntu" must have sudo permissions so that commands can be run without entering a password. 
The user "builder" must be created without sudo permissions. 
The non-sudo users "breaker", "client", and "server" are not required, but may be useful. 

The [executioner](TODO...) script must be installed at `/usr/bin/executioner`. 
This allows us to detect timeouts while commands are run on virtual machine. 
The VM image must install all the dependencies contestants need to compile their submissions. 
Here is an [example image](TODO...) we've used in the past, which uses a Dockerfile to create a Docker Swarm image. 


Oracle Submissions
------------------

Oracle submissions allow contestants to query a reference implementation to figure out expected behavior. 
The first argument to the grader script specifies a filepath location of a JSON file. 
The `type` key in the JSON file has the string value `"oracle"`. 
The `input` key contains whatever JSON the user submitted on the website.
Here's an example: 

	{
		"type": "oracle",
		"input": ["whatever","JSON","the user","submitted in the web form"]
	}

Output from `grader` must be provided as JSON to stdout.
The output JSON must have the `result` key set to the boolean value of whether the oracle ran successfully. 
If the oracle ran successfully, the `output` key must be set to the JSON value output of the oracle. 
If the oracle did not run successfully, the `error` key must be set to a string error message.
Here's an example output of a successful oracle run:

	{
		"result": true,
		"output": {"some":"JSON","output":"from oracle"}
	}

Here's an example output of an unsuccessful oracle run:

	{
		"result": false,
		"error": "some error message"
	}

When the `runner` receives an oracle submission, it does the following:

- Starts a VM instance.
- Sends all files in the problem directory to `/problem` on the VM.
- Uploads and generates the JSON input file. 
- Runs the grader (`/problem/grader`) as user "ubuntu" where the first argument is a filepath to a JSON file. 
- Parses the resulting JSON output from stdout and records the result in the database.


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



