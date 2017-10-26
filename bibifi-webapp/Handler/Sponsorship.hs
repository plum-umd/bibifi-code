module Handler.Sponsorship where

import Import

getSponsorshipR :: Handler Html
getSponsorshipR = runLHandler $ defaultLayout $ lLift $ do
    setTitle "Sponsorship Opportunities"
    [whamlet'|
        <div class="row">
            <div class="col-md-12">
                <div class="page-header">
                    <h1>
                        Sponsorship Opportunities
                <p class="lead">
                    Are you interested in sponsoring the contest? 
                    Sponsors are featured on our website and cost $1,000 for one year. 
                    Contact us at #{emailLink} if you'd like to learn more! 
    |]
--                     Are you interested in sponsoring the contest? 
--                     <a href="@{StaticR doc_sponsorship_pdf}">This document</a> describes various sponsorship opportunities for organizations and corporations. 
--                     If you are interested in participating in the break-it round as a professional team, see <a href="@{StaticR doc_professional_breaker_pdf}">here</a> for details. 
--                     We are also looking for judges for the contest. 
--                     See <a href="@{StaticR doc_judging_rules_pdf}">here</a> for details and responsibilities. 
--                     Feel free to contact us at #{emailLink} if you'd like to learn more! 
