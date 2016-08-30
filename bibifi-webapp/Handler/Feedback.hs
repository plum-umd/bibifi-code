module Handler.Feedback where

import Import

getFeedbackR :: Handler Html
getFeedbackR = runLHandler $ defaultLayout $ lLift $ do
    setTitle "Feedback"
    [whamlet'|
        <div class="row">
            <div class="col-md-12">
                <div class="page-header">
                    <h1>
                        Feedback
                <p>
                    Do you have any feedback about the contest? Have you found a bug or vulnerability in the contest's infrastructure? Let us know! Email us at #{emailLink}.
    |]
