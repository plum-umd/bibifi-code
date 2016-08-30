-- Deprecated. No longer available.

module Handler.About where

import Import

getAboutR :: Handler Html
getAboutR = defaultLayout $ do
    setTitle "About"
    cigital <- newIdent
    sponsors <- newIdent
    mc2 <- newIdent
    center <- newIdent
    toWidget [lucius|
        .#{center} {
            text-align: center;
        }
        ##{cigital} {
            width: 50%;
            height: 50%;
        }
        ##{mc2} {
            width: 70%;
            height: 70%;
            margin: 3px 0px;
        }
        ##{sponsors} {
            padding: 50px 0px;
        }
    |]
    [whamlet|
        <div class="row">
            <div class="col-md-12">
                <div class="page-header">
                    <h1>
                        About
                <p>
                    The Build it Break it Fix it security contest aims to teach students to write more secure programs.
                    The contest evaluates participants' abilities to develop secure and efficient programs. 
                    The contest is broken up into three rounds that take place over consecutive weekends. 
                    During the build it round, builders write software that implements the system prescribed by the contest.
                    In the break it round, breakers find as many flaws as possible. 
                    During the fix it round, builders attempt to fix any problems found by breaker teams.
                <p>
                    The BIBIFI contest is run by the Programming Languages Lab at the University of Maryland in conjunction with the Maryland Cybersecurity Center. 
                    The contest also serves as a research study in the area of cybersecurity. 
                <p>
                    We'd like to thank our sponsors for their generous support.
        <div class="row" id="#{sponsors}">
            <div class="col-md-6 #{center}">
                    <a href="http://www.cigital.com/">
                        <img id="#{cigital}" src="@{StaticR img_cigital_png}">
            <div class="col-md-6 #{center}">
                    <a href="http://www.cyber.umd.edu/">
                        <img id="#{mc2}" src="@{StaticR img_mc2_png}">
    |]

