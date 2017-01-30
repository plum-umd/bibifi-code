module Handler.Admin.Team.Member where

import qualified Admin
import Import

data FormData = FormData ()

    -- Check if on team.
    -- Form to set as leader.
    -- Form to remove from team.

generateHtml teamId userId teamAndUserE leaderFormM removeFormM = do
    Admin.setTitle "Manage User"
    ( teamName, userIdent) <- getNames
    [whamlet|
        <a href="@{AdminTeamR teamId}" type="button" class="btn btn-primary">
            Back
        <h2>
            Manage User
        <form class="form-horizontal">
            <div class="form-group">
                <label class="col-sm-2 control-label">
                    Team
                <div class="col-sm-10">
                    <p class="form-control-static">
                        #{teamName}
                <label class="col-sm-2 control-label">
                    User
                <div class="col-sm-10">
                    <p class="form-control-static">
                        #{userIdent}
    |]
    teamLeaderWidget
    removeWidget

    where
        getNames = do
            username <- fmap userIdent $ handlerToWidget $ runDB $ get404 userId
            case teamAndUserE of
                Left (team, _) ->
                    return ( teamName team, username)
                Right (team, _) -> 
                    return ( teamName team, username)

        teamLeaderWidget = do
            [whamlet|
                <h3>
                    Make Team Leader
            |]
            case teamAndUserE of
                Left _ ->
                    [whamlet|
                        <p>
                            This user is already the team leader.
                    |]
                Right _ -> do
                    (leaderW, leaderE) <- maybe 
                        (handlerToWidget $ generateFormPost leaderForm) 
                        return 
                        leaderFormM
                    [whamlet|
                        <form method=post action=@{AdminTeamMemberR teamId userId} enctype=#{leaderE} .form-basic role="form">
                            ^{leaderW}
                    |]

        removeWidget = do
            [whamlet|
                <h3>
                    Remove from team
            |]
            case teamAndUserE of
                Left _ ->
                    [whamlet|
                        <p> 
                            This user is the team leader. To remove this user from this team, make someone else the team leader first.
                    |]
                Right _ -> do
                    (widget, enctype) <- maybe
                        (handlerToWidget $ generateFormPost removeForm)
                        return
                        removeFormM
                    [whamlet|
                        <form method=post action=@{AdminTeamMemberR teamId userId} enctype=#{enctype} .form-basic role="form">
                            ^{widget}
                    |]

leaderForm = identifyForm "team-leader-form" $ renderBootstrap3 BootstrapBasicForm $ FormData
    <$> pure ()
    <*  bootstrapSubmit (BootstrapSubmit ("Make team leader"::Text) "btn-primary" [])

removeForm = identifyForm "remove-form" $ renderBootstrap3 BootstrapBasicForm $ FormData
    <$> pure ()
    <*  bootstrapSubmit (BootstrapSubmit ("Remove from team"::Text) "btn-danger" [])

getAdminTeamMemberR :: TeamId -> UserId -> Handler Html
getAdminTeamMemberR teamId userId = runLHandler $ Admin.layout Admin.Teams $ do
    teamAndUserE <- handlerToWidget $ runChecks teamId userId
    generateHtml teamId userId teamAndUserE Nothing Nothing

postAdminTeamMemberR :: TeamId -> UserId -> Handler Html
postAdminTeamMemberR teamId userId = runLHandler $ Admin.layout Admin.Teams $ do
    teamAndUserE <- handlerToWidget $ runChecks teamId userId
    case teamAndUserE of
        Left _ ->
            generateHtml teamId userId teamAndUserE Nothing Nothing
        Right teamAndUser -> do
            error "TODO: DEBUG THIS. ALWAYS ACCEPTING FIRST FORM. XXX"
            runMultipleFormsPost [
                FormAndHandler leaderForm (leaderFormHandler teamAndUser)
              , FormAndHandler removeForm (removeFormHandler teamAndUser)
              ]

    where
        removeFormHandler _ FormMissing _ _ = 
            error "Unreachable"
        removeFormHandler tau (FormFailure _) widget enctype = 
            generateHtml teamId userId (Right tau) Nothing (Just ( widget, enctype))
        removeFormHandler (_, Entity memberId _) (FormSuccess _) _ _ = do
            -- Remove from team in database.
            handlerToWidget $ runDB $ delete memberId

            -- Set message.
            setMessage [shamlet|
                <div class="container">
                    <div class="alert alert-success">
                        Removed user from team.
            |]

            -- Redirect.
            redirect $ AdminTeamR teamId

        leaderFormHandler _ FormMissing _ _ = 
            error "Unreachable"
        leaderFormHandler tau (FormFailure _) widget enctype =
            generateHtml teamId userId (Right tau) (Just ( widget, enctype)) Nothing
        leaderFormHandler (team, Entity memberId _) (FormSuccess _) _ _ = do
            -- Swap roles in database.
            handlerToWidget $ runDB $ do
                delete memberId
                insert_ $ TeamMember teamId (teamLeader team) 
                update teamId [TeamLeader =. userId]

            -- Set message.
            setMessage [shamlet|
                <div class="container">
                    <div class="alert alert-success">
                        Set as team leader.
            |]

            -- Redirect.
            redirect $ AdminTeamMemberR teamId userId

-- Check if user is on team. Returns left if user is the team leader.
runChecks teamId userId = do
    -- Check if leader.
    team <- runDB $ get404 teamId
    if teamLeader team == userId then do
        user <- runDB $ get404 userId
        return $ Left ( team, user)
    else do
        member <- runDB $ getBy404 $ UniqueTeamMember teamId userId
        return $ Right ( team, member)

