module Handler.Support (getSupportR) where

import Import
import Css
import Text.Julius (rawJS)

getSupportR :: Handler Html
getSupportR = runLHandler $ defaultLayout $ lLift $ do
    --sidebarStyle <- newIdent
    let sidebarStyle = "sidebar" :: Text
    setTitle "Support"
    sidebarCss sidebarStyle
    toWidget [lucius|
        .answer {
            margin-bottom: 8px;
        }
    |]
    toWidget [julius|
        $('body').scrollspy({ target: '##{rawJS sidebarStyle}' })
    |]
    [whamlet'|
        <div class="row">
            <div class="col-md-3">
                <div id="#{sidebarStyle}" data-spy="affix" data-offset-top="277">
                    <ul class="nav">
                        <li class="active">
                            <a href="#support">
                                Support
                            <ul class="nav">
                                <li>
                                    <a href="#faq">
                                        FAQ
                                <li>
                                    <a href="#contact">
                                        Contact us
                                    
            <div class="col-md-9">
                <div class="page-header">
                    <h1 id="support">
                        Support
                <h2 id="faq">
                    Frequently asked questions
                <dl>
                    <dt>
                        Are there prizes?
                    <dd class="answer">
                        Yes! Our sponsors have generously donated prizes. These prizes differ for each contest so be sure to check out the announcements page to see what you can win.
                        <!--
                        No, there are no prizes for the contest run through the Coursera capstone. 
                        -->
                    <dt>
                        How many members can be on a team?
                    <dd class="answer">
                        You can have as many members on your team as you want! You have to split the prizes between your team though.
                        <!--
                        Teams are limited to five members for the Coursera capstone. 
                        -->
                    <dt>
                        Can I participate?
                    <dd class="answer">
                        Eligibility requirements to participate in the contest can be found <a href="@{DetailsR}#eligibility">here</a>.
                    <dt>
                        Where will the contest take place?
                    <dd class="answer">
                        The contest will take place online so you can participate anywhere there is internet access.
                    <dt>
                        Can my team participate as both a builder and a breaker?
                    <dd class="answer">
                        Yes. When your team signs up for a contest, your team is automatically signed up as a builder and a breaker. Your builder and breaker scores are independent though, so you can choose to participate as a builder, a breaker, or both. 
                    <dt>
                        How is the contest scored? 
                    <dd class="answer">
                        You can find all the scoring information <a href="@{DetailsR}#scoring">here</a>.
                    <dt>
                        How do I sign up?
                    <dd class="answer last">
                        In order to participate in the contest, you need to:
                        <ol>
                            <li>
                                <a href="@{RegisterR}">Create an account</a>.
                            <li>
                                <a href="@{CreateTeamR}">Create a team</a>, or join a team (your team leader can invite you).
                            <li>
                                If you are the team leader, <a href="@{ContestSignupR}">sign your team up for the contest</a>.
                    <dt>
                        How do I reset my password?
                    <dd class="answer">
                        Email us at #{emailLink} to request a password reset.
                <h2 id="contact">
                    Contact us
                <p>
                    Still have questions or need help? Email us at #{emailLink}.
    |]
