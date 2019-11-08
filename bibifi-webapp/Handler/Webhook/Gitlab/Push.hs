module Handler.Webhook.Gitlab.Push (postWebhookGitlabPushR) where

import PostDependencyType
    ( ContestRound(..)
    , BuildSubmissionStatus(..)
    , BreakSubmissionStatus(..)
    , FixSubmissionStatus(..)
    )
import Import as I
import Data.Maybe (fromMaybe)
import Data.Aeson as J

postWebhookGitlabPushR :: Handler ()
-- Handles Gitlab's call when there is a push.
-- Right now, there's no way to authenticate that the call is from Gitlab.
-- We only treat the call as a ping, and check back with Gitlab to retrieve information
-- regarding the commit hash the call claims about.
postWebhookGitlabPushR = runLHandler $ do
    PushMsg _ _ tName cmts <- requireJsonBody :: LHandler PushMsg
    liftIO $ print $ "Repo name is " ++ show tName
    Entity cId c  <- withErr "No default contest" defaultContest
    Entity tcId _ <- runDB $ do
        let no tag = "No " ++ tag ++ " for "  ++ show tName
        Entity tId _ <- withErr (no "team") $ getBy (UniqueTeam tName)
        withErr (no "team contest") $ getBy (UniqueTeamContest tId cId)
    mapM_ (insertSubmission c tcId) cmts
  where
    withErr msg = (return . fromMaybe (error msg) =<<)

insertSubmission :: Contest -> TeamContestId -> Commit -> LHandler ()
-- Insert a submission of the right round in the database for given team and commit-hash
insertSubmission c tcId cmt = do
    t <- getCommitTimestamp
    case roundForTime t c of
        Just r -> (case r of
                       ContestRoundBuild -> insertBuild
                       ContestRoundBreak -> insertBreak
                       ContestRoundFix   -> insertFix) t
        Nothing -> reject
  where
    h = commitHash cmt
    insertBuild t = submit $
        BuildSubmission tcId t h BuildPending Nothing Nothing
    insertBreak t = do
        tcId' <- getTargetTeam cmt
        submit $ BreakSubmission tcId tcId' t h BreakPending Nothing False Nothing "" Nothing Nothing Nothing Nothing Nothing
    insertFix t = submit $
        FixSubmission tcId t h 0 FixPending Nothing "" Nothing Nothing Nothing
    submit = void . runDB . insert
    reject = undefined
    -- FIXME: timestamp is tricky:
    -- - Commit timestamps can be faked in git messages
    -- - Calling "now" on server risks being slightly late and unfairly rejecting a submission.
    --   Maybe we'll have some tolerance when computing the rounds.
    getCommitTimestamp = getCurrentTime
    getTargetTeam _ = undefined
    roundForTime t (Contest _ _ bld0 bld1 brk0 brk1 fix0 fix1)
        | bld0 <= t && t <= bld1 = Just ContestRoundBuild
        | brk0 <= t && t <= brk1 = Just ContestRoundBreak
        | fix0 <= t && t <= fix1 = Just ContestRoundFix
        | otherwise              = Nothing

-- Extracted information from the JSON message
data PushMsg = PushMsg
    { projectId :: Int
    , userName :: Text
    , repoName :: Text -- TODO: repository.name == team name?
    , commits :: [Commit]
    } deriving (Eq,Show)
data Commit = Commit
    { commitHash :: Text
    , commitTimestamp :: Text
    } deriving (Eq,Show)

instance J.FromJSON PushMsg where
    parseJSON = J.withObject "PushEvent" $ \o ->
        PushMsg <$> o .: "project_id"
                <*> o .: "user_username"
                <*> ((o .: "repository") >>= (.: "name"))
                <*> ((o .: "commits") >>= parseJSON)
instance J.FromJSON Commit where
    parseJSON = J.withObject "Commit" $ \o ->
        Commit <$> o .: "id"
               <*> o .: "timestamp"
