module Handler.CreateTeam ( getCreateTeamR, postCreateTeamR) where

import Import
import Forms
import Team

data TeamPost = TeamPost Text [Text]

teamForm :: Form TeamPost
teamForm = renderBootstrap3' $ TeamPost
    <$> areq identityField "Team name" Nothing
    <*> areq listEmailField "Team members" Nothing

generateHtml :: Widget -> Enctype -> [Text] -> LHandler Html
generateHtml widget enctype msg =
    let title = generatePageTitle Nothing "Create a team" in
    defaultLayout $ do 
        setTitle $ toHtml title
        let msgH = mconcat $ map displayError msg
        [whamlet|$newline never
<div class="row">
    <div class="col-md-12">
        <div class="page-header">
            <h1>
                Create a team
        ^{msgH}
        <form method=post action=@{CreateTeamR} enctype=#{enctype}>
            ^{widget}
            <div class="form-group">
                <div class="col-sm-offset-2 col-sm-10">
                    <button type="submit" class="btn btn-default">
                        Submit
|]

getCreateTeamR :: Handler Html
getCreateTeamR = runLHandler $ do
    -- Force log in.
    _ <- requireAuthId
    ( widget, enctype) <- generateFormPost teamForm
    generateHtml widget enctype []

postCreateTeamR :: Handler Html
postCreateTeamR = runLHandler $ do
    -- Force log in.
    uId <- requireAuthId
    ((res, widget), enctype) <- runFormPost teamForm
    case res of
        FormSuccess (TeamPost name members) -> 
            let team = Team name uId in
            do
            res' <- runDB $ insertUnique team
            case res' of
                Nothing -> 
                    generateHtml widget enctype ["Sorry, that team name already exists."]
                Just teamId -> do
                    inviteTeam teamId members
                    setMessage [shamlet|
                        <div class="container">
                            <div class="alert alert-success">
                                Successfully created team! Invitations have been emailed to your team members. 
                                <strong>
                                    Be sure to sign your team up for the contest!
                    |]
                    redirect ContestSignupR

        FormFailure _msg ->
            generateHtml widget enctype [] -- msg
        FormMissing ->
            generateHtml widget enctype []
    
