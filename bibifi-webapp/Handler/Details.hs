module Handler.Details where

import Import
import Text.Julius (rawJS)

getDetailsR :: Handler Html
getDetailsR = runLHandler $ defaultLayout $ lLift $ do
    setTitle "Contest Details"
    sidebarStyle <- newIdent
    sidebarCss sidebarStyle
    toWidget [julius|
        $('body').scrollspy({ target: '##{rawJS sidebarStyle}' })
    |]
    [whamlet'|
        <div class="row">
            <div class="col-md-3">
                <div id="#{sidebarStyle}" data-spy="affix" data-offset-top="277">
                    <ul class="nav">
                        <li class="active">
                            <a href="#details">
                                Contest Details
                            <ul class="nav">
                                <!-- <li>
                                    <a href="#prizes">
                                        Prizes
                                -->
                                <li>
                                    <a href="#overview">
                                        Overview
                                <li>
                                    <a href="#registration">
                                        Registration
                                <!--
                                <li>
                                    <a href="#location">
                                        Location
                                <li>
                                    <a href="#eligibility">
                                        Eligibility
                                <li>
                                    <a href="#teams">
                                        Teams
                                -->
                                <li>
                                    <a href="#scoring">
                                        Scoring
                                <li>
                                    <a href="#rules">
                                        Rules
            <div class="col-md-9">
                <div class="page-header">
                    <h1 id="details">
                        Contest Details
                <!--
                <h2 id="prizes">
                    Prizes
                <p>
                    Our sponsors have generously donated prizes! 
                    Prizes for the Fall 2014 contest will be <strong>$4000</strong> for each first place team and <strong>$2500</strong> for each second place team. 
                    Prizes must be shared between team members.
                -->
                <h2 id="overview">
                    Overview
                <p>
                    The build-it, break-it (BIBI) contest takes place over two rounds. Each round is roughly two weeks in length.
                <ul>
                    <li>
                        Round 1: Build-it teams build software according to a problem specification that is announced on the day the contest begins. To qualify, this software must pass a set of correctness tests that we will provide. It is scored based on its performance, and on how many optional features it implements.
                    <li>
                        Round 2: Break-it teams are given access to the source code of build-it submissions and they attempt to find correctness and security bugs (where the definition of the latter is problem specific). They submit test cases that provide evidence of these bugs, and these are vetted by our submission system. Teams receive points for each bug they find, and more points for security-relevant bugs.
                    <!--
                    <li>
                        Round 3: Build-it teams are given the test cases for bugs found against their submission, and may fix those bugs. If fixing a bug causes multiple test cases to pass (as is likely) then build-it teams earn points back for the duplicate tests (and break-it teams lose the corresponding points). 
                <p>
                    Prizes are awarded at the end of the third round to the top three build-it
                    teams, and the top three break-it teams. (Teams can win prizes in both
                    categories.) Prizes are also awarded to lower-scoring teams, where the chance of
                    winning a prize increases with the team's <a href="#scoring">score</a>. -->
                    <!-- See the scoring section for details.-->
                <h2 id="registration">
                    Registration
                <p>
                    To participate in the contest, you need to:
                <ol>
                    <li>
                        <a href="@{RegisterR}">Create an account</a>.
                    <li>
                        <a href="@{CreateTeamR}">Create a team</a>, or join a team (your team leader can invite you).
                    <li>
                        If you are the team leader, <a href="@{ContestSignupR}">sign your team up for the contest</a>.
                <!--
                <h2 id="location">
                    Location
                <p>
                    The contest takes place online so you can participate anywhere there is internet access. 
                <h2 id="eligibility">
                    Eligibility
                <p>
                    You can participate in the contest if:
                <ol>
                    <li>
                        You are enrolled as a graduate or undergraduate student at an accredited university, anywhere in the world, during the same calendar year that the contest takes place.
                    <li>
                        You do not have close relationships with any of the contest organizers.
                -->
                <!--
                    <li>
                        You are either a US citizen or attend a US-based University. 
                -->

                <!--
                    Build it Break it will be used for the <a href="https://www.coursera.org/specialization/cybersecurity/7?utm_medium=courseDescripTop">Coursera capstone</a>. 
                    The only restriction for this iteration of the contest is that you must be enrolled in the Coursera capstone. 
                -->
                <!--
                <h2 id="teams">
                    Teams
                <p>
                    Non-Coursera teams can have as many members as they want, however any prizes will be split between the team members. 
                    If you are looking for teammates, let us know at #{emailLink}. We'll help you find other people to compete with. 
                <p>
                    Teams are limited to five members for the Coursera capstone. 
                    If you are looking for teammates, please fill out the survey on Coursera and coordinate on the Coursera forums. 
                <p>
                    When your team signs up for a contest, your team is automatically signed up as a builder and a breaker. 
                    Your builder and breaker scores are independent though, so you can choose to participate as a builder, a breaker, or both. 
                -->
                <h2 id="scoring">
                    Scoring
                <p>
                    Builder teams and breaker teams are scored <em>separately</em>, but you are welcome to compete as both. Only builder teams participate in the Build-it and Fix-it phases. Breaker teams participate in the Break-it phase. 
                <p>
                    A <em>bug</em> is defined as incorrect behavior by a program, while a <em>vulnerability</em> is defined as a security exploit in a program. 
                    A <em>crash</em> is defined as unexpected program termination due to violations of memory safety. 
                <p>
                    Scoring will continue after each round ends as judges review submissions and performance results are updated. We will announce once final scores are available. 
                <h3>
                    Scoring for Builder Teams
                <h4>
                    Build-it Round
                <ol>
                    <li>
                        <strong>
                            Eligibility tests (250 pts total).
                        <p>
                            All builder teams' submissions must pass a set of tests to be eligible. If you correctly pass all tests, you get <strong>250 points</strong>. 
                                /*
                                <ul>
                                    <li> <em>Correctness tests</em> #{dash} These test the correctness of basic features.
                                    <li> <em>Performance tests</em> #{dash} These are larger tests that do more substantial work. For each test case, you get up to <strong>M extra points</strong> depending on how efficient your submission is. The amount of points you get is scaled linearly based on the fastest and slowest submissions. For example, if your submission takes 15 seconds to run, the fastest submission takes 10 seconds to run, and the slowest submission takes 20 seconds to run, you would receive <strong>M/2 points</strong>. Tying submissions get an equal number of points, and timeouts receive no points. 
                                */
                    <li>
                        <strong>
                            Optional tests (25 pts each + performance bonuses).
                        <p>
                            These tests aren't necessary for eligibility, but they're a great way to get extra points. 
                            There are two types:
                                <ul>
                                    <li> <em>Correctness tests</em> #{dash} These test the correctness of specification features. Each is worth <strong>25 points</strong> each.
                                    <li> <em>Performance tests</em> #{dash} These are larger tests that do more substantial work. For each test case, you get up to <strong>50 extra points</strong> depending on how efficient your submission is. The amount of points you get is scaled linearly based on the fastest and slowest submissions. For example, if your submission takes 15 seconds to run, the fastest submission takes 10 seconds to run, and the slowest submission takes 20 seconds to run, you would receive <strong>25 points</strong>. Tying submissions get an equal number of points, and timeouts receive no points. 
                            
                            
                <h4>
                    Break-it Round
                <p>
                    For every <em>unique</em> bug found against your submission during the Break-it phase, you will lose <strong>25 points</strong>. 
                    For every <em>unique</em> bug found against your submission that leads to a crash, you will lose <strong>50 points</strong>. 
                    For every <em>unique</em> vulnerability found during the Break-it phase, you will lose <strong>100 points</strong>. (For what "unique" means, see the Fix-it scoring below.)
                <h4>
                    Fix-it Round
                <p>
                    This is the chance for builders to prove that the bugs (and vulnerabilities) in their code found by the breaker teams correspond to the same bug (or vulnerability). If a builder can prove that multiple bugs (and vulnerabilities) are the same, the builder team will <strong>gain points back</strong> such that they are only penalized for the bug (or vulnerability) once.
                <p>
                    To show that multiple reported bugs are actually the "same," submit a single, small code patch that fixes the bug. Judges will uses these fixes to determine which bugs are morally the same.
                <p>
                    Be careful to only fix one unique bug (or vulnerability) at a time. You will be <strong>penalized</strong> if you fix more than one unique bug (or vulnerability) at a time. 
                <h3>
                    Scoring for Breaker Teams
                <h4>
                    Build-it Round
                <p>
                    Breakers receive <strong>25</strong> points for identifying a bug in the oracle implementation. 
                    Points will only be awarded to the first breaker team that reports the bug. 
                    To report a bug in the oracle implementation, email us with a description of the bug, links to relevant oracle submissions, your team name, and team id. 
                    /*Bugs in the oracle implementation may only be reported for points during the build-it round. */
                <h4>
                    Break-it Round
                <p>
                    The total number of points received for any bug found is <strong>25</strong>. 
                    The points from a given bug are split evenly among all the breaker teams who find it. 
                    The points from bugs that lead to crashes are similar except the total number of points is <strong>50</strong>. 
                    The points from vulnerabilities are similar except the total number of points is <strong>100</strong>.
                <p>
                    <em>Example</em> #{dash} Each bug would fetch up to 25 points. If there were 5 breaker teams who found a given bug, then they would each get 5 points; if you are the only team to find a given bug, you'd get the full 25 points.
                <h4>
                    Fix-it Round
                <p>
                    Builder teams will be unifying bugs and vulnerabilities so expect your score to go down if you found the same bugs and vulnerabilities as other breaker teams. 
                <h2 id="rules">
                    Rules
                <h4>
                    General
                <ul>
                    <li>
                        Do not hack the contest's infrastructure, or you will be disqualified. 
                    <li>
                        A minimum number of 30 registered teams, 10 teams that pass required build-it tests, and 10 teams that submit a break-it-bug are required to award prizes to participants. 
                    <li>
                        Teams must do their own work and not share solutions with others. 
                    <li>
                        Contest organizers reserve the right to use their discretion in making judgements. 
                <h4>
                    Build-it
                <ul>
                    <li>
                        Do not obfuscate the source code in your submissions and answer all survey questions, or you will be disqualified. 
                    <li>
                        Make sure that your build-it submissions do not time out. 
                        You will be given 10 minutes before a single test times out and 30 minutes for all tests to complete before timing out.
                    <li>
                        Beware of changing your build submission between the build-it and fix-it rounds because if you make changes, they will be considered as part of your first fix, which may end up disqualifying it.
                <h4>
                    Break-it
                <ul>
                    <li>
                        You will only be allowed to make 5 submissions against a single team. 
                    <!--
                    <li>
                        You will only be allowed to make one integrity submission and one confidentiality submission against a single team. 
                    -->
                <h4>
                    Fix-it
                <ul>
                    <li>
                        Only fix a single bug (or vulnerability) at a time. 
    |]
    -- Text from fall 2014:
    -- For every <em>unique</em> bug found against your submission during the Break-it phase, you will lose <strong>M/2 points</strong>. For every <em>unique</em> vulnerability found during the Break-it phase, you will lose <strong>M points</strong>. (For what "unique" means, see the Fix-it scoring below.)






--                 <p>
--                     You can participate in the contest if:
--                 <ol>
--                     <li>
--                         You are enrolled as a graduate or undergraduate student anytime during the same calendar year that the contest takes place.
--                     <li>
--                         You are either a US citizen or attend a US-based University. 
--                     <li>
--                         You do not have close relationships with any of the contest organizers. 
--                 <p>
--                     We may require proof of these requirements from contestants that may take the form of university ids, unofficial transcripts, etc. Close relationships are defined as:
--                 <ul>
--                     <li>
--                         Immediate family members and close personal friends. 
--                     <li>
--                         People with an established working relationship; this would include students who have done independent studies with the organizers or their research group and/or worked for them as research and/or teaching assistants.
