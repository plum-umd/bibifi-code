{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Announcements ( getAnnouncementsR, getSpecificAnnouncementsR) where

import Import
import qualified Team
-- import System.Locale

announcement :: Entity Post -> Widget -- HtmlUrl url -- Handler (HtmlUrl url)
announcement post = -- TODO
    let Entity _ p = post in
    let content = postContent p in
    let postStyle = "post-ident" :: Text in --newIdent
    do
    toWidget [lucius|
        .#{postStyle} {
            margin-bottom: 20px;
        }
    |]
    t <- lift $ displayTime $ postTimestamp p
    [whamlet'|
<div class="#{postStyle}">
    <h4>
        #{postTitle p}
        <div>
            <small>
                #{t}
    <div>
        #{content}
|]

showAnnouncements :: Entity Contest -> LWidget -- Handler (HtmlUrl url)
showAnnouncements c = 
    let Entity cId contest = c in
    let url = contestUrl contest in
    do
    announcements <- handlerToWidget $ runDB $ selectList [PostContest ==. cId, PostDraft ==. False] [Desc PostTimestamp] -- TODO, filter less than now, drafts
    let posts = case announcements of
          [] -> [whamlet'|$newline never
            <div class="row">
                <div class="col-md-12">
                    There are no posts at this time.
          |]
          as -> 
              let p = mconcat $ map announcement as in
              [whamlet'|
                <div class="row">
                    <div class="col-md-12">
                        ^{p}
              |]
    buildStart <- lLift $ lift $ displayTime $ contestBuildStart contest
    buildEnd <- lLift $ lift $ displayTime $ contestBuildEnd contest
    breakStart <- lLift $ lift $ displayTime $ contestBreakStart contest
    breakEnd <- lLift $ lift $ displayTime $ contestBreakEnd contest
    -- Check if contest has already started.
    now <- getCurrentTime
    button <- do
          uIdM <- handlerToWidget $ maybeAuthId
          case uIdM of
            Nothing -> 
                return $ if now >= (contestBuildStart contest) then
                  [whamlet'|
                      <div class="signup">
                          <a href=@{ContestParticipationR url} class="btn btn-danger btn-block">
                              PARTICIPANTS
                  |]
                else do
                  [whamlet'|
                      <div class="signup">
                          <a href=@{ContestSpecificSignupR url} class="btn btn-danger btn-block">
                              SIGN UP
                  |]
            Just uId -> do
                tc' <- handlerToWidget $ runDB $ Team.getTeamContest uId cId
                -- lLift $ lift $ putStrLn $ show tc'
                return $ case tc' of
                    [tcId] ->
                        [whamlet'|
                            <div class="signup">
                                <a href=@{ParticipationInformationR tcId} class="btn btn-danger btn-block">
                                    PARTICIPANTS
                        |]
                    _ ->
                        [whamlet'|
                            <div class="signup">
                                <a href=@{ContestSpecificSignupR url} class="btn btn-danger btn-block">
                                    SIGN UP
                        |]
    [whamlet|
        <div class="row">
            <div class="col-md-9">
                ^{posts}
            <div class="col-md-3">
                <div class="panel panel-default">
                    <div class="panel-heading">
                        <h3 class="panel-title">
                            Dates
                    <div class="panel-body">
                        <div>
                            <strong>
                                Build It Round
                            <p class="text-muted">
                                <small>
                                    #{buildStart} - #{buildEnd}
                            <strong>
                                Break It Round
                            <p class="text-muted">
                                <small>
                                    #{breakStart} - #{breakEnd}
                ^{button}
    |]

getAnnouncements :: Maybe Text -> LHandler Html
getAnnouncements contestId = do
    contest <- retrieveContest contestId
    let title = generatePageTitle contest "Announcements"
    customLayout contest $ do
        setTitle $ toHtml title
        toWidget [lucius|
.signup {
    text-align: center;

    .btn {
        padding: 8px 48px;
    }
}
|]
        contestTemplate contest "Announcements" showAnnouncements
        
getAnnouncementsR :: Handler Html
getAnnouncementsR = runLHandler $
    getAnnouncements Nothing

getSpecificAnnouncementsR :: Text -> Handler Html
getSpecificAnnouncementsR contestId = runLHandler $
    getAnnouncements (Just contestId)

