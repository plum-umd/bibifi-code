module Handler.Thanks where

import Contest
import Import

generateFall2014 :: LWidget
generateFall2014 = do
    cigital <- newIdent
    mc2 <- newIdent
    patriot <- newIdent
    nsf <- newIdent
    cyberpoint <- newIdent
    tob <- newIdent
    att <- newIdent
    suprtek <- newIdent
    astech <- newIdent
    galois <- newIdent
    isec <- newIdent
    toWidget [lucius|
        ##{cyberpoint} {
            margin: 20px 0px;
        }
        ##{cigital} {
            width: 270px;
            height: 86px;
            margin: 27px 0px 16px;
        }
        ##{mc2} {
            width: 378px;
            height: 80px;
            margin: 125px 0px;
        }
        ##{isec} {
            width: 300px;
            height: 300px;
            margin: 15px 0px;
        }
        ##{patriot} {
            width: 250px;
            height: 88px;
            margin: 16px 0px 25px;
        }
        ##{nsf} {
            margin: 20px 0px;
        }
        ##{tob} {
            width: 360px;
            height: 200px;
            margin: 20px 0px;
        }
        ##{att} {
            width: 315px;
            height: 182px;
        }
        ##{suprtek} {
            width: 442px;
            height: 349px;
        }
        ##{astech} {
            margin: 8px 0px;
        }
        ##{galois} {
            margin: 12px 0px;
        }
        .sponsors {
        }
        .spacer {
            margin-top: 35px;
            margin-bottom: 35px;
        }
    |]
    [whamlet|
        <p .lead>
            We would like to thank the following organizations and individuals for their support in helping make this contest possible. 
        <h3>
            Judges
        <p>
            Stephen Chong (Harvard University), John Regehr (University of Utah), Stephen Magill (Galois), Ian Blumenfeld (Galois), Aslan Askarov (Aarhus University), Jeff Kuhn (Amches), Lok Yan (AFRL), Vu Le (University of California, Davis), Alvaro Ugaz (SuprTEK), Jay Ruhnke (SuperTEK), Andre Protas (CyberPoint), Bryon Fryer, Carl Steinebach, Austin Parker, Aaron Temin (Mitre), and Sue Wang (Mitre)
        <h3>
            Professional Break-it Teams
        <p>
            CyberPoint and AT&T (gs1)
        <h3>
            Students
        <p>
            <strong>
                Specification tester: 
            Nicholas Labich and Luís Pina
        <p>
            <strong>
                Website design and development: 
            Kristopher Micinski and Paige Nelson
        <h3>
            Testing Infrastructure
        <p>
            Amazon EC2
        <h3>
            Gold Sponsors
        <div class="row">
            <div class="sponsors col-md-12 col-lg-offset-2 col-lg-8 text-center">
                <a href="http://cyberpointllc.com/">
                    <img id="#{cyberpoint}" src="@{StaticR img_cyberpoint_png}">
        <div class="row">
            <div class="sponsors col-md-12 col-lg-offset-2 col-lg-8 text-center">
                <a href="http://www.trailofbits.com/">
                    <img id="#{tob}" src="@{StaticR img_tob_jpg}">
        <div class="row">
            <div class="sponsors col-md-12 col-lg-offset-2 col-lg-8 text-center">
                <a href="http://www.att.com/">
                    <img id="#{att}" src="@{StaticR img_att_jpg}">
        <div class="row">
            <div class="sponsors col-md-12 col-lg-offset-2 col-lg-8 text-center">
                <a href="http://www.suprtek.com/">
                    <img id="#{suprtek}" src="@{StaticR img_suprtek_png}">
        <h3>
            Silver Sponsors
        <div class="row">
            <div class="sponsors col-sm-6 col-lg-offset-1 col-lg-5 text-center">
                <a href="https://www.isecpartners.com/">
                    <img id="#{isec}" src="@{StaticR img_iSec_jpg}">
            <div class="sponsors col-sm-6 col-lg-5 text-center">
                <a href="http://www.cyber.umd.edu/">
                    <img id="#{mc2}" src="@{StaticR img_mc2_png}">
        <h3>
            Bronze Sponsors
        <div class="row">
            <div class="sponsors col-sm-6 col-lg-offset-1 col-lg-5 text-center">
                <a href="http://www.cigital.com/">
                    <img id="#{cigital}" src="@{StaticR img_cigital_png}">
            <div class="sponsors col-sm-6 col-lg-5 text-center">
                <a href="http://patriot-tech.com/">
                    <img id="#{patriot}" src="@{StaticR img_patriot_png}">
        <div class="row">
            <div class="sponsors col-sm-6 col-lg-offset-1 col-lg-5 text-center">
                <a href="https://www.astechconsulting.com/">
                    <img id="#{astech}" src="@{StaticR img_astech_png}">
            <div class="sponsors col-sm-6 col-lg-5 text-center">
                <a href="http://galois.com/">
                    <img id="#{galois}" src="@{StaticR img_galois_png}">
        <div class="row">
            <div class="sponsors col-sm-offset-3 col-sm-6 col-lg-offset-3 col-lg-6 text-center">
                <a href="http://www.nsf.gov/">
                    <img id="#{nsf}" src="@{StaticR img_nsf_png}">
    |]

getThanksR :: Text -> Handler Html
getThanksR cUrl = runLHandler $ do
    contest <- retrieveContest $ Just cUrl
    let header = "Special Thanks"
    let title = generatePageTitle contest header
    customLayout contest $ do
        setTitle $ toHtml title
        contestTemplate contest header $ \_ -> case cUrl of
            "fall2014" ->
                generateFall2014
            _ ->
                notFound

