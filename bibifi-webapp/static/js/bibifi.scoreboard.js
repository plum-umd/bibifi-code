bibifi = {};
bibifi.scoreboard = function() {
	// Private
	var url = "";
	var delay = 30;

	var defaultBuilderScore = -100000;
	var defaultBreakerScore = 0;
	var maxBuilder = defaultBuilderScore;
	var minBuilder = defaultBuilderScore;
	var maxBreaker = defaultBreakerScore;
	var minBreaker = defaultBreakerScore;

	var cushion = 10;

	var builderView = $("#builder-scores");
	var breakerView = $("#breaker-scores");

	var checkNull = function( x) {
		return ( x === null) ? "&#8212;" : x;
	};

	var parseDate = function( date) {
		var res = d3.time.format.utc("%Y-%m-%dT%H:%M:%S.%LZ").parse( date);
		if ( !res)
			res = d3.time.format( "%Y-%m-%dT%H:%M:%SZ").parse( date);

		return res;
	};

	var printDate = d3.time.format( "%b %e");
	
	// // D3 variables
	// var margin = {top: 20, right: 80, bottom: 30, left: 50},
	// 	width = 960 - margin.left - margin.right,
	// 	height = 500 - margin.top - margin.bottom;
	// 
	// var x = d3.time.scale()
	// 	.range([0, width]);
	// 
	// var y = d3.scale.linear()
	// 	.range([height, 0]);
	// 
	// // var color = d3.scale.category10();

	// var xAxis = d3.svg.axis()
	// 	.tickFormat( printDate)
	// 	.scale(x)
	// 	.orient("bottom");
	// 
	// var yAxis = d3.svg.axis()
	// 	.scale(y)
	// 	.orient("left");

	// var line = d3.svg.line()
	// 	.interpolate("basis")
	// 	.x( function(d) { return x(d.date);})
	// 	.y( function(d) { return y(d.value);});

	// var svg = d3.select("body").append("svg")
	// 	.attr("width", width + margin.left + margin.right)
	// 	.attr("height", height + margin.top + margin.bottom)
	// .append("g")
	// 	.attr("transform", "translate(" + margin.left + "," + margin.top + ")");

	var json_callback = function( data) {
		// Helper functions
		
		var idToName = function( id) {
			return data.meta.teams[id].name;
		};

		var isProfessional = function( id) {
			return data.meta.teams[id].professional;
		}

		// Parse dates
		data.meta.start = parseDate( data.meta.start);
		data.meta.end = parseDate( data.meta.end);

		// console.log( "success");
		// console.log( data);
		// console.log( data.meta.teams[0]);
		// tmp = data;

		// Initialize scores
		var buildScores = {};
		var breakScores = {};
		for ( var tId in data.meta.teams) {
			// Check if professional.
			if ( !isProfessional( tId)) {
				buildScores[tId] = {
					id: tId,
					name: idToName( tId),
					values: [{
						date: data.meta.start,
						value: defaultBuilderScore,
					}],
					latest: {
						buildScore: defaultBuilderScore,
						breakScore: null,
						fixScore: null,
					},
				};
			}

			breakScores[tId] = {
				name: idToName( tId),
				values: [{
					date: data.meta.start,
					value: defaultBreakerScore,
				}],
				latest: {
                                        buildScore: defaultBreakerScore,
					breakScore: null,
					fixScore: null,
				},
				professional: isProfessional( tId),
			};
		}

		// Update builder scores
		for ( var i in data.builder) {
			var team = data.builder[i].team;
			var time = parseDate( data.builder[i].timestamp);
			var buildS = data.builder[i].buildScore;
			var breakS = data.builder[i].breakScore;
			var fixS = data.builder[i].fixScore;

			// Check that a build score for that team exists
			if ( buildScores[team]) {
				if ( !buildS)
					buildS = defaultBuilderScore;

				// Compute new score
				var score = buildS + breakS + fixS;
				var value = {
					date: time,
					value: score,
				};

				maxBuilder = Math.max( maxBuilder, score);
				minBuilder = Math.min( minBuilder, score);
				
				// Set latest score
				var latest = {
					buildScore: buildS,
					breakScore: breakS,
					fixScore: fixS,
				};

				buildScores[team].values.push( value);
				buildScores[team].latest = latest;
			}
			else {
				// This should never happen
				console.log( "error: build score does not exist for the given team");
			}
		}

		// Update breaker scores
		for ( var i in data.breaker) {
			var team = data.breaker[i].team;
			var time = parseDate( data.breaker[i].timestamp);
			var buildS = data.breaker[i].buildScore;
			var breakS = data.breaker[i].breakScore;
			var fixS = data.breaker[i].fixScore;

			// Check that break score for that team exists
			if ( breakScores[team]) {
				if ( !buildS)
					buildS = defaultBreakerScore;

				// Compute new score
				var score = buildS + breakS + fixS;
				var value = {
					date: time,
					value: score,
				};

				maxBreaker = Math.max( maxBreaker, score);
				minBreaker = Math.min( minBreaker, score);

				// Set latest score
				var latest = {
					buildScore: buildS,
					breakScore: breakS,
					fixScore: fixS,
				};

				breakScores[team].values.push( value);
				breakScores[team].latest = latest;
			}
			else {
				// This should never happen
				console.log( "error: break score does not exist for the given team");
			}
		}

		// Append data points for now
		var now = new Date();
		for ( var tId in data.meta.teams) {

			// Set builder score
			if ( buildScores[tId]) {
				var score = buildScores[tId].latest.buildScore
					+ buildScores[tId].latest.breakScore
					+ buildScores[tId].latest.fixScore;

				buildScores[tId].values.push( {
					date: now,
					value: score,
				});
			}
			else {
				// This should never happen
				console.log( "error: build score does not exist for the given team");
			}

			// Set breaker score
			if ( breakScores[tId]) {
				var score = breakScores[tId].latest.buildScore
                                        + breakScores[tId].latest.breakScore
					+ breakScores[tId].latest.fixScore;
				
				breakScores[tId].values.push( {
					date: now,
					value: score,
				});
			}
			else {
				// This should never happen
				console.log( "error: build score does not exist for the given team");
			}
		}

		// Convert to arrays and sort
		var scoreCompare = function( a, b) {
			var aS = a.latest.breakScore + a.latest.fixScore;
			if ( a.latest.buildScore)
				aS += a.latest.buildScore;

			var bS = b.latest.breakScore + b.latest.fixScore;
			if ( b.latest.buildScore)
				bS += b.latest.buildScore;

			if ( aS > bS)
				return -1;
			if ( aS < bS)
				return 1;
			return 0;
		}

		buildScores = d3.values(buildScores).sort( scoreCompare);
		breakScores = d3.values(breakScores).sort( scoreCompare);

		// Clear out divs
		builderView.empty();
		breakerView.empty();

		// // Create builder graph
		// x.domain( [ data.meta.start, data.meta.end]);
		// y.domain( [ minBuilder - cushion, maxBuilder + cushion]);

		// svg.append("g")
		// 	.attr("class", "x axis")
		// 	.attr("transform", "translate(0," + height + ")")
		// 	.call(xAxis);

		// svg.append("g")
		// 	.attr("class", "y axis")
		// 	.call(yAxis)
		// .append("text")
		// 	.attr("transform", "rotate(-90)")
		// 	.attr("y", 6)
		// 	.attr("dy", ".71em")
		// 	.style("text-anchor", "end")
		// 	.text("Builder Score");

		// // console.log( buildScores);
		// var city = svg.selectAll(".builder-point")
		// 		.data( buildScores)
		// 	.enter()
		// 		.append("g")
		// 		.attr("class", "builder-point");
	
		// city.append("path")
		// 		.attr("class", "line")
		// 		.attr("d", function(d) { return line(d.values); })


		// hovering??


		//		.style("stroke", function(d) { return color(d.name); });
	
		// city.append("text")
		// 		.datum(function(d) { return {name: d.name, value: d.values[d.values.length - 1]}; })
		// 		.attr("transform", function(d) { return "translate(" + x(d.value.date) + "," + y(d.value.temperature) + ")"; })
		// 		.attr("x", 3)
		// 		.attr("dy", ".35em")
		// 		.text(function(d) { return d.name; });

		// replace builder div html
		// 
		// same for breaker

		//console.log( buildScores);
		//console.log( breakScores);


		// Display builder table
		var table = $('<table class="table table-hover"></table>');
		table.append( '<thead><tr><th>Team</th><th>Build-it Round</th><th>Break-it Round</th><th class="fixit">Fix-it Round</th><th>Total Score</th></tr></thead>');
		var tBody = $('<tbody></tbody>');
		for ( var i in buildScores) {
			var id = buildScores[i].id;
			var name = buildScores[i].name;
			var buildS = buildScores[i].latest.buildScore;
			var breakS = buildScores[i].latest.breakScore;
			var fixS = buildScores[i].latest.fixScore;
			var s = buildS + breakS + fixS;

			tBody.append( '<tr class="clickable" href="/scoreboard/'+id
				+'/breakdown"><td>'+name
				+'</td><td>'+checkNull( buildS)
				+'</td><td>'+checkNull( breakS)
				+'</td><td class="fixit">'+checkNull( fixS)
				+'</td><td>'+checkNull( s)
				+'</td></tr>');
		}
		table.append( tBody);
		builderView.append( table);

		// Display breaker table
		table = $('<table class="table table-hover"></table>');
		table.append( '<thead><tr><th>Team</th><th>Build-it Round</th><th>Break-it Round</th><th class="fixit">Fix-it Round</th><th>Total Score</th></tr></thead>');
		var tBody = $('<tbody></tbody>');
		for ( var i in breakScores) {
			var name = breakScores[i].name;
			var buildS = breakScores[i].latest.buildScore;
			var breakS = breakScores[i].latest.breakScore;
			var fixS = breakScores[i].latest.fixScore;
			var s = buildS + breakS + fixS;

			// Check if professional.
			if ( breakScores[i].professional)
				name = name + "<sup>*</sup>";

			tBody.append( '<tr><td>'+name
				+'</td><td>'+checkNull( buildS)
				+'</td><td>'+checkNull( breakS)
				+'</td><td class="fixit">'+checkNull( fixS)
				+'</td><td>'+checkNull( s)
				+'</td></tr>');
		}
		table.append( tBody);
		breakerView.append( table);

		// Register clickables.
		$(".clickable").click(function() {
		  window.document.location = $(this).attr("href");
		});

		// Update again after delay
		setTimeout( bibifi.scoreboard.update, delay * 1000);
	};

	// Public
	return {
		init: function( newUrl) {
			url = newUrl;

			this.update();
		},

		update: function() {
			// Start ajax request
			$.getJSON( url, json_callback).fail( function() {
				// TODO
				console.log( "error: failed to load scores");
			});

		},
	};
}();
