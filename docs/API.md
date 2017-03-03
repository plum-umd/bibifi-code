Contest Problem API
===================

You can integrate your own problem specifications into the BIBIFI infrastructure by using this Contest Problem API. 
Contest problems require a Docker Swarm or AWS EC2 virtual machine image and a grading script that can run oracle submissions, build-it submissions, and break-it submissions. 
The grading script must accept JSON input and output as described in this document. 
If a submission takes too long to run, the system will time out and the submission will be rejected. 
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

Every time a builder pushes a new commit, the infrastructure automatically grades the build submission against a suite of correctness, perfomance, and optional tests. 
The grader script is run on the VM and must conform to the following specification to run build-it tests. 
The first argument to the grader script specifies a filepath location of a JSON file. 
The `type` key in the JSON file has the string value `"build"`. 
The `input` key contains the JSON test script for the current test. 
You can set the test script through the admin page of the website (Click on the current contest, "view, create, and edit build-it tests", and select or create the test). 
The `port` key is an integer that provides a unique unused port in case your tests need to open ports. 
The port is incremented by 20 for each test. 
Here is an example input file: 

	{
		"type": "build",
		"input": {"some":"testcase","expected":"output?"},
		"port": 6300
	}

Output from `grader` must be provided as JSON to stdout.
The output JSON must have the `result` key set to the boolean value of whether the test passed. 
The `error` key is an optional string that can be used to provide an error message for the user. 
The `time` key is a number that indicates how long the test took to run (typically measured in seconds). 
Here is an example output:

	{
		"result": false,
		"error": "some error message",
		"time": 3.114704
	}

When the `runner` receives a build-it submission, it does the following:

- Starts a VM instance.
- Uploads and untars build submissions to `/home/builder/submission`.
- Compiles the build submission with `sudo -i -u builder make -B -C /home/builder/submission/build`.
- Uploads all files in the problem directory to `/problem`.
- Runs each build test:
				- Uploads the JSON test input.
				- Runs the grader (`/problem/grader`) as user "ubuntu" where the first argument is a filepath to a JSON file.
				- Parses the resulting JSON output from stdout.
- Records the result in the database.

Break Submissions
-----------------

Breakers submit tests against builder code during the break-it round. 
The grader script is run on the VM to grade submitted breaks. 
The first argument to the grader script specifies a filepath location of a JSON file. 
The `type` key in the JSON file has the string value `"break"`. 
The `classification` key has the string values `"correctness"`, `"integrity"`, `"confidentiality"`, `"crash"`, or `"security"` depending on the kind of the break test.
The `port` key provides a unique unused port in case your tests need to open ports. 
The `test` key is the JSON break test submitted by the breaker. 
Here is an example input file: 

	{
		"type": "build",
		"classification": "confidentiality",
		"port": 6300,
		"test": ["some","break","test"]
	}

Output from `grader` must be provided as JSON to stdout.
The output JSON must have the optional `result` key set to the boolean value of whether the break test demonstrated a violation. 
If the `result` key is omitted, the grader script indicates that it cannot automatically evaluate the break test, so it should be marked for manual judgement. 
The `error` key is an optional string that can be used to provide an error message for the breaker when `result` is false. 

	{
		"result": false,
		"error": "some error message"
	}

When the `runner` receives a break-it submission, it does the following:

- Parses the JSON break-it test and makes sure a text description file exists.
- Starts a VM instance.
- Uploads all files in the problem directory to `/problem/`.
- Uploads and decompresses the builder's final build submission to `/home/builder/submission`.
- Compiles the build submission with `sudo -i -u builder make -B -C /home/builder/submission/build`.
- Uploads all files in the break test directory (`<repository path>/repos/<breaker team id>/break/<break name>/`) to `/break/`.
- Uploads the JSON test input file.
- Runs the grader (`/problem/grader`) as user "ubuntu" where the first argument is a filepath to a JSON file.
- Parses the resulting JSON output from stdout and records the result in the database.

Fix Submissions
---------------

During fix-it, builders have the opportunity to fix bugs found during the break-it round. 
Builders submit fixes as new commits to their git repositories, along with the list of breaks they have fixed. 
The `runner` does the following when it receives a fix-it submission: 

- Retrieve all break submissions that were automatically tested (status is BreakTested).
- Make sure a text file exists describing the break. 
- Start a VM instance. 
- Uploads and decompresses the fix submission to `/home/builder/submission`.
- Compiles the build submission with `sudo -i -u builder make -B -C /home/builder/submission/fix/code/build`.
- Run all required build-it tests. 
- Run all automatically tested break-it tests.
- If all the tests passed, mark the submission for judgement.

The infrastructure runs all required build-it tests and automatically tested break-it tests. 
Contest administrators are responsible for passing final judgement on whether non-automatically tested break-it tests are fixed and whether the break-it tests are due to a single underlying bug. 

