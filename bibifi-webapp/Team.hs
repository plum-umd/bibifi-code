module Team (inviteTeam, Page(..), layout, getTeam, getTeamContest, getTeamMembers) where

import Import as I
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding
import qualified Text.Blaze.Html.Renderer.String as S (renderHtml)
import Text.Shakespeare.Text (stext)
import Network.Mail.Mime

sendInvitation :: Text -> Text -> LHandler ()
sendInvitation email code = do
    renderer <- getUrlRenderParams
    let url = T.pack $ S.renderHtml $ [hamlet|@{TeamInvitationR code}|] renderer
    let to = [Address Nothing email]
    let head = [("Subject", "Accept a team invitation")]
    let text = Data.Text.Lazy.Encoding.encodeUtf8 [stext|
You have received a team invitation to participate in the Build it Break it Fix it contest. Click the link below to accept.

\#{url}
|]
    let textPart = Part { 
        partType = "text/plain; charset=utf-8",
        partEncoding = None,
        partDisposition = DefaultDisposition,
        partContent = PartContent text,
        partHeaders = []
    }
    let html = renderHtml [shamlet|
        <p>You have received a team invitation to participate in the Build it Break it Fix it contest. Click the link below to accept. 
        <p>
            <a href=#{url}>#{url}
    |]
    let htmlPart = Part { 
        partType = "text/html; charset=utf-8",
        partEncoding = None,
        partDisposition = DefaultDisposition,
        partContent = PartContent html,
        partHeaders = []
    }
    liftIO $ renderSendMail (emptyMail $ Address (Just "Build it Break it Fix it") "noreply@builditbreakit.org")
                { mailTo = to, mailHeaders = head, mailParts = [[textPart, htmlPart]] }

inviteTeam :: TeamId -> [Text] -> LHandler ()
inviteTeam _ [] = return ()
inviteTeam teamId members = 
    let h:t = members in
    do
    code <- liftIO $ I.randomString 20
    let invite = TeamInvite code teamId h
    res'' <- runDB $ insertUnique invite
    case res'' of 
        Nothing ->
            inviteTeam teamId members
        Just _ -> do
            sendInvitation h code
            inviteTeam teamId t

-- (Label l, m ~ HandlerT site IO) => ReaderT (YesodPersistBackend site) (LMonadT l m) a
getTeamContest :: (MonadIO m) => UserId -> ContestId -> ReaderT SqlBackend m [Key TeamContest]
getTeamContest uId cId =
    --do {res_0 <- E.select (E.from (\(E.LeftOuterJoin (E.InnerJoin _team _teamcontest) _teammember) -> do {E.on (E.just (_team E.^. TeamId) E.==. _teammember E.?. TeamMemberTeam);
    --        E.on (_team E.^. TeamId E.==. _teamcontest E.^. TeamContestTeam);
    --        E.where_ (_teamcontest E.^. TeamContestContest E.==. E.val cId);
    --        return (_teamcontest E.^. TeamContestContest,
    --                _teamcontest E.^. TeamContestTeam,
    --                _teammember E.?. TeamMemberTeam,
    --                _team E.^. TeamId,
    --                _teamcontest E.^. TeamContestId)}));
    --mapM (\(_teamcontest_contest,
    --        _teamcontest_team,
    --        _teammember_team,
    --        _team_id,
    --        _teamcontest_id) -> do return (_teamcontest_id)) res_0}
    [lsql|
        select TeamContest.id from Team
        inner join TeamContest on Team.id == TeamContest.team
        left outer join TeamMember on Team.id == TeamMember.team
        where TeamContest.contest == #{cId}
            and (Team.leader == #{uId} or TeamMember.user == #{Just uId})
        limit 1
    |]
--     E.select $ E.from $ \( t `E.LeftOuterJoin` tm, tc) -> do
--         E.on ( t E.^. TeamId E.==. tm E.^. TeamMemberTeam)
--         E.where_ ( tc E.^. TeamContestTeam E.==. t E.^. TeamId E.&&. ( t E.^. TeamLeader E.==. E.val uId E.||. tm E.^. TeamMemberUser E.==. E.val uId) E.&&. tc E.^. TeamContestContest E.==. E.val cId)
--         E.limit 1
--         return ( tc E.^. TeamContestId)

-- Get the team. If the user is not on that team, redirect to not found. 
getTeam :: TeamId -> LHandler Team
getTeam tId = do
    teamM <- runDB $ get tId
    case teamM of
        Nothing -> do
            setMessage [shamlet|
                <div class="container">
                    <div class="alert alert-danger">
                        Team not found.
            |]
            redirect AnnouncementsR
        Just team -> do
            taintLabel $ (dcSingleton $ PrincipalGroup tId) `glb` dcSingleton PrincipalAdmin
            return team

-- getTeamMembers :: (PersistQuery m, MonadResource m, PersistMonadBackend m ~ E.SqlBackend) => 
--     TeamId -> m [UserId]
getTeamMembers tId = do
    teamM <- get tId
    case teamM of
        Nothing ->
            return []
        Just team -> do
            members <- selectList [TeamMemberTeam ==. tId] []
            return $ (teamLeader team):(map (\(Entity _ (TeamMember _ uId)) -> uId) members)

data Page = Information | ContestParticipation | AddMembers | Leave -- | TODO:Members
layout :: Page -> TeamId -> (UserId -> Team -> LWidget) -> LHandler Html
layout page tId content =
    let informationActive = case page of 
          Information -> "active" :: Text
          _ -> ""
    in
    let contestActive = case page of 
          ContestParticipation -> "active" :: Text
          _ -> ""
    in
    let addMembersActive = case page of
          AddMembers -> "active" :: Text
          _ -> ""
    in
    let leaveActive = case page of
          Leave -> "active" :: Text
          _ -> ""
    in
    let subtitle = case page of
          Information -> "Information" :: Text
          ContestParticipation -> "Contest Participation"
          AddMembers -> "Add Members"
          Leave -> "Leave Team"
    in
    do
    uId <- requireAuthId
    raiseGroupLabel
    team <- getTeam tId
    let addMembers = if uId == ( teamLeader team) then
            [whamlet|
                <li class="#{addMembersActive}">
                    <a href="@{TeamAddMembersR tId}">
                        Add Members
            |] :: LWidget
        else
            mempty
    let leave = if uId /= teamLeader team then
            [whamlet|
                <li class="#{leaveActive}">
                    <a href="@{TeamLeaveR tId}">
                        Leave Team
            |] :: LWidget
          else
            mempty
    defaultLayout $ do
        setTitle [shamlet|#{subtitle} - Team|]
        let content' = content uId team
        [whamlet|
            <div class="row">
                <div class="col-md-12">
                    <div class="page-header">
                        <h1>
                            Team
                            <small>
                                #{subtitle}
            <div class="row">
                <div class="col-md-3">
                    <ul class="nav nav-pills nav-stacked">
                        <li class="#{informationActive}">
                            <a href="@{TeamInformationR tId}">
                                Information
                        <li class="#{contestActive}">
                            <a href="@{TeamParticipationR tId}">
                                Contest Participation
                        ^{addMembers}
                        ^{leave}
                <div class="col-md-9">
                    ^{content'}
        |]

