module Handler.TeamInvitation where

import Import as I
import Control.Monad (foldM)
import Data.Text as T

data Code = Code Text

invitationForm :: Maybe Text -> Form Code
invitationForm invitation = 
    let setting = FieldSettings "Invitation code" Nothing Nothing Nothing [("readonly","readonly")] in
    renderDivs $ Code
        <$> areq textField setting invitation

data State = 
      Invalid { error::Text}
    | Valid { code::Text, team_name::Text}

generateHtml :: Widget -> Enctype -> [Text] -> State -> LHandler Html
generateHtml widget enctype msg state = 
    let title = generatePageTitle Nothing "Join a team" in
    defaultLayout $ do 
        setTitle $ toHtml title
        case state of 
            Invalid err ->
                [whamlet|
                    <div class="row">
                        <div class="col-md-12">
                            <div class="page-header">
                                <h1>
                                    Join a team
                            <div class="text-danger">
                                #{err}
                |]
            Valid invitation name -> do
                header <- return $ T.append "Join team: " $ name
                msgH <- return $ mconcat $ I.map displayError msg
                [whamlet|
                    <div class="row">
                        <div class="col-md-12">
                            <div class="page-header">
                                <h1>
                                    #{header}
                            ^{msgH}
                            <form method=post action=@{TeamInvitationR invitation} enctype=#{enctype} class="form-horizontal" role="form">
                                ^{widget}
                                <input type=submit value="Join team">
                |]

generateHtmlHelper :: Widget -> Enctype -> [Text] -> Text -> LHandler Html
generateHtmlHelper widget enctype msg invitation = do
    res <- runDB $ getBy $ UniqueTeamInvite invitation
    case res of 
        Nothing ->
            generateHtml widget enctype [] $ Invalid "Sorry, this invitation code is invalid."
        Just (Entity _ invite) -> do
            res' <- runDB $ get $ teamInviteTeam invite
            case res' of 
                Nothing ->
                    generateHtml widget enctype [] $ Invalid "Sorry, this team doesn't exist."
                Just team ->
                    generateHtml widget enctype msg $ Valid invitation $ teamName team


getTeamInvitationR :: Text -> Handler Html
getTeamInvitationR invitation = runLHandler $ do
    -- Force log in.
    _ <- requireAuthId
    ( widget, enctype) <- generateFormPost $ invitationForm $ Just invitation
    generateHtmlHelper widget enctype [] invitation

postTeamInvitationR :: Text -> Handler Html
postTeamInvitationR invitation = runLHandler $ do
    -- Force log in.
    uId <- requireAuthId
    ((res, widget), enctype) <- runFormPost $ invitationForm Nothing
    case res of
        FormSuccess (Code code') -> do
            res' <- runDB $ getBy $ UniqueTeamInvite code'
            case res' of 
                Nothing ->
                    -- Invite doesn't exist. 
                    generateHtml widget enctype [] $ Invalid "Sorry, this invitation code is invalid."
                Just (Entity inviteId invite) -> 
                    let teamId = teamInviteTeam invite in
                    do
                    res'' <- runDB $ get $ teamId
                    case res'' of
                        Nothing ->
                            -- Team doesn't exist.
                            generateHtml widget enctype [] $ Invalid "Sorry, this team doesn't exist."
                        Just team -> do
                            -- Check if user is the team leader.
                            if uId == teamLeader team then
                                generateHtml widget enctype [] $ Invalid "You are the team leader."
                            else do
                                -- Check if user can join team by checking user and team against all contests that have not finished yet.
                                now <- getCurrentTime
                                contests <- runDB $ selectList [ContestFixEnd >=. now] []
                                join <- foldM (\acc contest@(Entity contestId _) -> case acc of
                                    Left _ ->
                                        return acc
                                    Right () ->
                                        userCanJoinTeamForContest uId teamId contestId
                                    ) (Right ()) contests
                                case join of
                                    Left err ->
                                        generateHtml widget enctype [] $ Invalid err -- "You are already a member of a team participating in an upcoming contest."
                                    Right () -> do
                                        -- Add user to team.
                                        res''' <- runDB $ insertUnique $ TeamMember teamId uId
                                        case res''' of 
                                            Nothing -> 
                                                -- User is already a member of team.
                                                generateHtml widget enctype [] $ Invalid "You are already a member of this team."
                                            Just _ -> do
                                                -- Delete invitation.
                                                runDB $ delete inviteId
                                                setMessage [shamlet|
                                                    <div class="container">
                                                        <div class="alert alert-success">
                                                            Successfully joined team "#{teamName team}"!
                                                |]
                                                redirectUltDest AnnouncementsR -- TODO: change to something else? a default?
        FormFailure _msg ->
            generateHtmlHelper widget enctype [] invitation -- msg
        FormMissing ->
            generateHtmlHelper widget enctype [] invitation
        
