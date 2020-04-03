module Handler.Admin.Team.AddMember where

import qualified Data.List as List

import qualified Admin
import Forms
import Import
import Team (getTeamMembers)

data FormData = FormData [Text]

makeForm = do
    usernames <- runDB $ [lsql|select ident from user|]

    return $ renderBootstrap3 (BootstrapHorizontalForm (ColSm 0) (ColSm 2) (ColSm 0) (ColSm 10)) $ FormData
        <$> areq (listTokenField usernames) (bfs' "Usernames") Nothing
        <*  bootstrapSubmit (BootstrapSubmit ("Add to team"::Text) "btn-primary" [])

generateHtml :: TeamId -> Maybe (Widget, Enctype) -> [Text] -> LWidget
generateHtml teamId formM errs = do
    Admin.setTitle "Add Team Members"
    form <- handlerToWidget makeForm
    (formW, formE) <- maybe
        (handlerToWidget $ generateFormPost form)
        return
        formM
    team <- handlerToWidget $ runDB $ get404 teamId
    [whamlet|
        <a href="@{AdminTeamR teamId}" type="button" class="btn btn-primary">
            Back
        <h2>
            Add Team Members
        <div>
            ^{errsW}
        <form method=post action="@{AdminTeamAddMemberR teamId}" enctype=#{formE} .form-horizontal role="form">
            <div class="form-group">
                <label class="col-sm-2 control-label">
                    Team
                <div class="col-sm-10">
                    <p class="form-control-static">
                        #{teamName team}
            ^{formW}
    |]

    where
        errsW = mconcat $ map (\err -> [whamlet|
            <p .text-warning>
                #{err}
          |]) errs


getAdminTeamAddMemberR :: TeamId -> Handler Html
getAdminTeamAddMemberR teamId = runLHandler $ Admin.layout Admin.Teams $ do
    generateHtml teamId Nothing []

postAdminTeamAddMemberR :: TeamId -> Handler Html
postAdminTeamAddMemberR teamId = runLHandler $ Admin.layout Admin.Teams $ do
    form <- handlerToWidget makeForm
    ((res, formW), formE) <- handlerToWidget $ runFormPost form
    case res of
        FormMissing ->
            generateHtml teamId (Just (formW, formE)) ["Invalid request."]
        FormFailure msg ->
            generateHtml teamId (Just (formW, formE)) msg
        FormSuccess (FormData usernames) -> do
            -- Lookup user ids for usernames.
            userIdMs <- handlerToWidget $ runDB $ mapM (getBy . UniqueUser) usernames
            case sequence userIdMs of
                Nothing -> 
                    generateHtml teamId (Just (formW, formE)) ["User does not exist."]
                Just userIds' -> do
                    let userIds = map entityKey userIds'

                    -- Check if users are already on the team.
                    teamMembers <- handlerToWidget $ runDB $ getTeamMembers teamId
                    if not (null $ List.intersect userIds teamMembers) then
                        generateHtml teamId (Just (formW, formE)) ["User is already on the team."]
                    else do

                        -- Add users to team.
                        handlerToWidget $ runDB $ mapM (insert_ . TeamMember teamId) userIds

                        -- Set message.
                        setMessage [shamlet|$newline never
                            <div class="container">
                                <div class="alert alert-success">
                                    Added team members.
                        |]

                        -- Redirect.
                        redirect $ AdminTeamR teamId
            

