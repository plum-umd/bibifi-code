module Handler.Splash where

import Import
import qualified Sponsors
import qualified Winners

jumbotronWidget :: Widget
jumbotronWidget = do
    toWidget [lucius|
        .splash {
            padding: 30px 0px;
            margin-bottom: 15px;
            font-size: 26px;
            font-weight: 200;
            line-height: 2.1428571435;
            color: inherit;

            .title {
                font-size: 48pt;
                line-height: 1.5;
            }

            p {
                line-height: 1.4;
            }

            .btn {
                margin-top: 5px;
                margin-bottom: 5px;
                background-color: #363636;
                border-color: #4b4b4b;
                color: #fff;
            }

            .btn:hover {
                background-color: #686868;
                border-color: #7c7c7c;
            }
        }
    |]
    [whamlet'|
        <div class="row">
            <div class="col-md-12">
                <div class="splash">
                    /*
                    <div class="title langdon">
                        BUILD IT, BREAK IT, FIX IT
                    */
                    <p>
                        /*Maryland Cybersecurity Center, AT&T, Cyberpoint, SuprTEK, and Trail of Bits present <strong>Build it Break it Fix it</strong>, a new security-oriented programming contest.*/
                        <a href="http://www.boozallen.com/">Booz Allen Hamilton</a> and <a href="http://www.cyber.umd.edu/">Maryland Cybersecurity Center</a> present <strong>Build it Break it Fix it</strong>, a new security-oriented programming contest.
                    /*
                    <p>
                        The Build it Break it Fix it contest will be used for the Coursera capstone. Find out more information about the capstone <a href="https://www.coursera.org/specialization/cybersecurity/7?utm_medium=courseDescripTop">here</a>! 
                    */
                    /*
                    <p>
                        <a class="btn btn-lg" role="button" href="@{ScoreboardR}">
                            Scoreboard
                    */
                    /*
                        This iteration of the contest is only available to undergraduate and graduate students enrolled in U.S. universities. 
                        <a href="@{ContestSignupR}">Sign up</a>! 
                        If you are participating in the Coursera capstone, sign up at <a href="https://coursera.builditbreakit.org/">coursera.builditbreakit.org</a> instead of on this website. 
                    <p>
                        The Build it Break it Fix it contest for fall 2015 is complete. Congratulations to the winners! Check out the <a href="@{SpecificScoreboardR "fall2015"}">scoreboard</a> for the results. 
                    */
                    <p>
                        If you are interested in the scientific results from this research project, check out a <a href="http://arxiv.org/abs/1606.01881">pre-print of our paper</a>.
                    <p>
                        The Build it Break it Fix it contest for <a href="@{StaticR doc_flyer2016_pdf}">Fall 2016</a> will start on September 22, 2016. 
                        <a href="@{ContestSignupR}">Sign up</a>! 
    |]

prizesWidget :: Widget
prizesWidget = [whamlet'|
        <div class="row info">
            <div class="col-xs-12 col-sm-9 col-md-10">
                <div class="splash-header">
                    <span>
                        Prizes
                    <img src="@{StaticR img_splash_line_png}" class="splash-line">
                <p>
                    Thanks to our sponsors for providing prizes for contestants! 
                    Prizes are awarded to teams with the top builder and breaker scores. 
                    The prize pool for the fall 2016 contest will be up to <b>$13,500</b>.
                    /*Prizes for the Fall 2015 contest are <b>$4000</b> for each first place team and <b>$2500</b> for each second place team. */
                    /*Prizes vary between each run of the contest, but can range from technical books to thousands of dollars.*/
                    Prizes must be shared between team members. 
                    There are no prizes for the Coursera capstone participants. 
            <div class="hidden-xs col-sm-3 col-md-2">
                <img src="@{StaticR img_prize_icon_svg}" class="icon pull-right">
    |]

aboutWidget :: Widget
aboutWidget = [whamlet'|
        <div class="row info">
            <div class="hidden-xs col-sm-3 col-md-2">
                <img src="@{StaticR img_about_icon_svg}" class="icon">
            <div class="col-xs-12 col-sm-9 col-md-10">
                <div class="splash-header">
                    <span>
                        About
                    <img src="@{StaticR img_splash_line_png}" class="splash-line">
                <p>
                    The Build it Break it Fix it security contest aims to teach students to write more secure programs. 
                    The contest evaluates participants' abilities to develop secure and efficient programs. 
                    The contest is broken up into three rounds that take place over consecutive weekends. 
                    During the Build It round, builders write software that implements the system prescribed by the contest. 
                    In the Break It round, breakers find as many flaws as possible in the Build It implementations submitted by other teams. 
                    During the Fix It round, builders attempt to fix any problems in their Build It submissions that were identified by other breaker teams.
                    <a href="@{ContestSignupR}">Sign up now</a>!
    |]
                    -- The next iteration of the contest will take place in Spring 2015. 
                    -- Each round will respectively start on <strong>August 28th</strong>, <strong>September 4th</strong>, and <strong>September 12th</strong>. 

detailsWidget :: Widget
detailsWidget = [whamlet'|
        <div class="row info">
            <div class="hidden-xs col-sm-3 col-md-2">
                <img src="@{StaticR img_details_icon_svg}" class="icon">
            <div class="col-xs-12 col-sm-9 col-md-10">
                <div class="splash-header">
                    <span>
                        Contest Details
                    <img src="@{StaticR img_splash_line_png}" class="splash-line">
                <p>
                    Contest participants have two independent scores, a builder score and a breaker score. 
                    These scores are separate, so teams are welcome to compete towards both or either. 
                    Detailed scoring information can be found <a href="@{DetailsR}#scoring">here</a>.
                    Student teams, worldwide, can participate. 
                    <!--
                    Eligible participants must be undergraduate or graduate students enrolled at an accredited university in the United States. 
                    -->
                    The contest takes place online so participants can compete anywhere with internet access. 
                    <a href="@{AnnouncementsR}">Click here</a> to view the latest announcements for the contest. 
                    Funding for the contest infrastructure is provided by the National Science Foundation. 
                    More details about the contest can be found <a href="@{DetailsR}">here</a>.
                    Check out blog posts by <a href="http://www.pl-enthusiast.net/?p=35">Michael Hicks</a> and <a href="https://blog.trailofbits.com/2014/07/30/education-initiative-spotlight-build-it-break-it/">Trail of Bits</a> discussing the contest. 
    |]
                    -- Check out Michael Hicks' <a href="http://www.pl-enthusiast.net/?p=35">blog post</a> or <a href="@{StaticR doc_slides_pdf}">these slides</a> about some of the motivation for this contest. 

sponsorWidget :: Widget
sponsorWidget = do
    mc2 <- newIdent
    tob <- newIdent
    nsf <- newIdent
    toWidget [lucius|
        ##{mc2} {
            width: 378px;
            height: 80px;
            margin: 80px 0px;
        }
        ##{nsf} {
            margin: 22px 0px;
        }
        ##{tob} {
            width: 360px;
            height: 200px;
            margin: 20px 0px;
        }
        .sponsors {
        }
        .spacer {
            margin-top: 35px;
            margin-bottom: 35px;
        }
    |]
    [whamlet'|
        <div class="row">
            <div class="col-md-12 col-lg-offset-2 col-lg-8">
                <hr class="spacer">
                <h1 class="text-center">
                    Platinum Sponsors
        <div class="row">
            <div class="sponsors col-md-12 col-lg-offset-2 col-lg-8 text-center">
                ^{Sponsors.booz}
        <!---
        <div class="row">
            <div class="col-md-12 col-lg-offset-2 col-lg-8">
                <hr class="spacer">
                <h1 class="text-center">
                    Gold Sponsors
        <div class="row">
            <div class="sponsors col-md-12 col-lg-offset-2 col-lg-8 text-center">
        -->
        <div class="row">
            <div class="col-md-12 col-lg-offset-2 col-lg-8">
                <hr class="spacer">
                <h1 class="text-center">
                    Silver Sponsors
        <div class="row">
            <div class="sponsors col-sm-6 col-lg-offset-1 col-lg-5 text-center">
                ^{Sponsors.leidos}
            <div class="sponsors col-sm-6 col-lg-5 text-center">
                ^{Sponsors.astech}
        <div class="row">
            <div class="sponsors col-sm-6 col-lg-offset-1 col-lg-5 text-center">
                <a href="http://www.trailofbits.com/">
                    <img id="#{tob}" src="@{StaticR img_tob_jpg}">
            /* <div class="sponsors col-sm-offset-3 col-sm-6 col-lg-offset-3 col-lg-6 text-center">*/
            <div class="sponsors col-sm-6 col-lg-5 text-center">
                <a href="http://www.cyber.umd.edu/">
                    <img id="#{mc2}" src="@{StaticR img_mc2_png}">

        <!---
        <div class="row">
            <div class="col-md-12 col-lg-offset-2 col-lg-8">
                <hr class="spacer">
                <h1 class="text-center">
                    Bronze Sponsors
        <div class="row">
            <div class="sponsors col-sm-6 col-lg-offset-1 col-lg-5 text-center">
                ^{Sponsors.lockheed}
            <div class="sponsors col-sm-6 col-lg-5 text-center">
                <a href="http://www.nsf.gov/">
                    <img id="#{nsf}" src="@{StaticR img_nsf_png}">
        -->
    |]

judgesWidget :: Widget
judgesWidget = [whamlet'|
        <div class="alert alert-success">
            We are currently recruiting judges for the contest. If interested, look <a href="@{SponsorshipR}">here</a> for more details. 
            <button type="button" class="close" data-dismiss="alert"><span aria-hidden="true">&times;</span><span class="sr-only">Close</span></button>
    |]

winnersWidget :: Widget
winnersWidget = Winners.fall2015 <> Winners.fall2015coursera
                        -- This <a href="http://www.pl-enthusiast.net/2014/10/08/bibifi-contest/">blog post</a> provides a more detailed recap of the contest. 

organizersWidget :: Widget
organizersWidget = do
    orgId <- newIdent
    toWidget [lucius|
        .#{orgId} {
            margin: 20px 0px;
            font-size: medium;
        }
    |]
    [whamlet'|
        <div class="row">
            <div class="col-md-12 col-lg-offset-2 col-lg-8">
                <hr class="spacer">
                <h1 class="text-center">
                    Organizers
        <div class="row">
            <div class="sponsors col-md-12 col-lg-offset-1 col-lg-10">
                <p .#{orgId}>
                    The Build it Break it Fix it competition was co-conceived and co-developed by <a href="http://www.cs.umd.edu/~awruef/">Andrew Ruef</a>, <a href="http://jamesparker.me/">James Parker</a>, and <a href="http://www.cs.umd.edu/~mwh/">Michael Hicks</a> with contributions from <a href="http://www.cs.umd.edu/~dml/">Dave Levin</a>, <a href="https://www.umiacs.umd.edu/~mmazurek/">Michelle Mazurek</a>, <a href="https://www.cs.umd.edu/~atif/">Atif Memon</a>, and <a href="https://www.cs.umd.edu/~jplane/">Jandelyn Plane</a>.
    |]

getSplashR :: Handler Html
getSplashR = runLHandler $
    defaultLayout $ lLift $ do
        setTitle "Build it Break it Fix it Contest"
        toWidget [lucius|
            .info .space {
                margin-top: 15px;
                margin-bottom: 15px;
            }

            .icon {
                margin-top: 12px;
                padding: 15px;
                width: 140px;
                height: 140px;
            }

            .splash-line {
                width: 100%;
                height: 28px;
                margin: -8px 0px 0px;
            }

            .splash-header {
                font-size: 36px;
                margin-top: 20px;
                margin-bottom: 10px;
                font-weight: 500;
                line-height: 1.1;
                overflow:hidden;
                white-space: nowrap;
            }
        |]
        -- TODO: vertically center icons?? 
        jumbotronWidget
        -- judgesWidget
        aboutWidget
        prizesWidget
        detailsWidget
        winnersWidget
        sponsorWidget
        organizersWidget
