module Handler.Webhook.Gitlab.Push (postWebhookGitlabPushR) where

import PostDependencyType
    ( BuildSubmissionStatus(..)
    , BreakSubmissionStatus(..)
    , FixSubmissionStatus(..)
    )
import Import as I
import Data.Maybe (fromMaybe)
import Data.Aeson as J

postWebhookGitlabPushR :: TeamContestId -> Text -> Handler ()
-- Handles Gitlab's call when there is a push.
-- Right now, there's no way to authenticate that the call is from Gitlab.
-- We only treat the call as a ping, and check back with Gitlab to retrieve information
-- regarding the commit hash the call claims about.
postWebhookGitlabPushR tcId _ = runLHandler $ do
    PushMsg _ _ _ cmts <- requireJsonBody :: LHandler PushMsg
    Entity _ c  <- withErr "No default contest" defaultContest
    mapM_ (insertSubmission c tcId) cmts
  where
    withErr msg = (return . fromMaybe (error msg) =<<)

insertSubmission :: Contest -> TeamContestId -> Commit -> LHandler ()
-- Insert a submission of the right round in the database for given team and commit-hash
insertSubmission c tcId cmt = do
    t <- getCommitTimestamp
    case c of
        Contest _ _ bld0 bld1 brk0 brk1
            | t `between` (bld0, bld1) -> insertBuild t
            | t `between` (brk0, brk1) -> undefined
            | otherwise                -> reject
  where
    h = commitHash cmt
    insertBuild t = submit $
        BuildSubmission tcId t h BuildPending Nothing Nothing
--    insertBreak t = do
--        tcId' <- getTargetTeam cmt
--        submit $ BreakSubmission tcId tcId' t h BreakPending Nothing False Nothing "" Nothing Nothing Nothing Nothing Nothing
--    insertFix t = submit $
--        FixSubmission tcId t h FixPending Nothing "" Nothing Nothing Nothing
    submit = void . runDB . insert
    reject = undefined
    -- FIXME: timestamp is tricky:
    -- - Commit timestamps can be faked in git messages
    -- - Calling "now" on server risks being slightly late and unfairly rejecting a submission.
    --   Maybe we'll have some tolerance when computing the rounds.
    getCommitTimestamp = getCurrentTime
    getTargetTeam _ = undefined
    between x (lo,hi) = lo <= x && x <= hi -- TODO: some tolerance?

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
