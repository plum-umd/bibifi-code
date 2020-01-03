module Handler.Team.Leave where

import Control.Monad
import qualified Data.Text.Lazy.Encoding
import Network.Mail.Mime
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Shakespeare.Text (stext)

import Import
import qualified Team

redirectTeamLeaders uId tId team = when (uId == teamLeader team) $ do
    setMessage [shamlet|
        <div class="container">
            <div class="alert alert-danger">
                Team leaders cannot leave their own team.
    |]
    redirect $ TeamInformationR tId
    
data FormData = FormData ()

leaveForm :: Form FormData
leaveForm = renderBootstrap3 BootstrapBasicForm $ FormData
    <$> pure ()
    <*  bootstrapSubmit (BootstrapSubmit ("Leave"::Text) "btn-danger" [])

-- generateHtml :: Widget -> Enctype -> LHandler Html
generateHtml tId widget enctype = 
    [whamlet|
        <h3>
            Leave team
        <p>
            Leave this team.
        <p .text-danger>
            Warning: You will need to be reinvited to the team if you wish to rejoin in the future.
        <p>
            <form method=post action=@{TeamLeaveR tId} enctype=#{enctype} class="form-horizontal" role="form">
                ^{widget}
    |]

getTeamLeaveR :: TeamId -> Handler Html
getTeamLeaveR tId = runLHandler $ Team.layout Team.Leave tId $ \uId team -> do
    -- Check if team leader.
    redirectTeamLeaders uId tId team

    (widget, enctype) <- handlerToWidget $ generateFormPost leaveForm
    generateHtml tId widget enctype

postTeamLeaveR :: TeamId -> Handler Html
postTeamLeaveR tId = runLHandler $ Team.layout Team.Leave tId $ \uId team -> do
    -- Check if team leader.
    redirectTeamLeaders uId tId team

    -- Check if in the middle of a contest.
    redirectIfContestOngoing tId

    -- Remove user.
    handlerToWidget $ runDB $ deleteBy $ UniqueTeamMember tId uId
    
    -- Email rest of team.
    emailTeam tId team uId

    -- Redirect to profile's team page.
    setMessage [shamlet|
        <div class="container">
            <div class="alert alert-success">
                Successfully left team #{teamName team}.
    |]
    redirect $ ProfileTeamsR

    where
        redirectIfContestOngoing tId = do
            now <- getCurrentTime
            cs <- handlerToWidget $ runDB [lsql| select Contest.* from TeamContest inner join Contest on TeamContest.contest == Contest.id where TeamContest.team == #{tId}|]
            mapM_ (\(Entity _ contest) ->
                when (now >= contestBuildStart contest && now <= contestBreakEnd contest) $ do
                    setMessage [shamlet|
                        <div class="container">
                            <div class="alert alert-danger">
                                Cannot leave a team during an ongoing contest.
                    |]
                    redirect $ TeamInformationR tId
              ) cs

        emailTeam tId team uId = do
            -- Get username.
            username <- do
                userM <- handlerToWidget $ runDB $ get uId
                return $ maybe "UNKNOWN" userIdent userM

            -- Get their email addresses.
            to <- handlerToWidget $ do
                members <- runDB [lsql| select User.email from User inner join TeamMember on TeamMember.user == User.id where TeamMember.team == #{tId} |]
                leaderM <- runDB $ get $ teamLeader team
                let emails = case leaderM of
                      Nothing ->
                        members
                      Just leader ->
                        (userEmail leader):members
                return $ map (Address Nothing) emails
            let head = [("Subject", "A team member has left your team")]
            let text = Data.Text.Lazy.Encoding.encodeUtf8 [stext|
Hi team #{teamName team},

Just to let you know, #{username} just left your team.

Best,

BIBIFI organizers
|]
            let textPart = Part { 
                partType = "text/plain; charset=utf-8",
                partEncoding = None,
                -- partFilename = Nothing,
                partDisposition = DefaultDisposition,
                partContent = PartContent text,
                partHeaders = []
            }
            let html = renderHtml [shamlet|
                <p>
                    Hi team #{teamName team},
                <p>
                    Just to let you know, #{username} just left your team. 
                <p>
                    Best,
                <p>
                    BIBIFI organizers
            |]
            let htmlPart = Part { 
                partType = "text/html; charset=utf-8",
                partEncoding = None,
                -- partFilename = Nothing,
                partDisposition = DefaultDisposition,
                partContent = PartContent html,
                partHeaders = []
            }
            mail <- initEmptyMail
            liftIO $ renderSendMail mail
                {mailTo = to, mailHeaders = head, mailParts = [[textPart, htmlPart]]}







