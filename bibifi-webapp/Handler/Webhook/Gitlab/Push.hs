module Handler.Webhook.Gitlab.Push (postWebhookGitlabPushR) where

import PostDependencyType
    ( BuildSubmissionStatus(..)
    , BreakSubmissionStatus(..)
    , FixSubmissionStatus(..)
    , BreakSubmissionResult(..)
    , FixSubmissionResult(..)
    )

import BreakSubmissions (withdrawBreakSubmission)
import Import
import Control.Exception.Enclosed
import Control.Monad.Trans.Maybe (MaybeT(..))
import qualified Data.Aeson as J
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import Data.List (isInfixOf)
import Data.Maybe (isNothing)
import Data.Text (pack)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import qualified GitLab as Gitlab
--import GitLab.Types as GT
import qualified GitLab.API.RepositoryFiles as Gitlab
import PostDependencyType (BreakType)
import System.FilePath.Posix (splitDirectories)

postWebhookGitlabPushR :: TeamContestId -> Text -> Handler ()
-- Handles Gitlab push notification.
-- There's no way to authenticate that the call is from Gitlab at the moment..
-- We only treat the call as a ping, and check back with Gitlab to retrieve information
-- regarding the commit hash.
postWebhookGitlabPushR tcId token = runLHandler $ do
    (cId, nonce, repoIdM) <- runDB $ do
        TeamContest _ cId _ _ _ nonce repoIdM <- get404 tcId
        return (cId, nonce, repoIdM)
    unless (token `constantCompareText` nonce) $
        permissionDenied ""
    pushTime <- getCurrentTime
    contest <- runDB $ get404 cId
    PushMsg pId cmts gitUrl <- requireJsonBody

    -- Set repo id and git url if repoIdM is missing.
    when (isNothing repoIdM) $ do
        runDB $ update tcId [TeamContestGitUrl =. gitUrl, TeamContestGitRepositoryIdentifier =. (Just $ GitlabRepositoryIdentifier pId)]

    mapM_ (handleCommit pushTime pId contest tcId) cmts

handleCommit :: UTCTime -> Int -> Contest -> TeamContestId -> Commit -> LHandler ()
-- Insert submission of the right kind (build/break/fix) into the database based on push time
handleCommit t pId (Contest _ _ bld0 bld1 brk0 brk1 fix1) tcId (Commit h added modified)
    | t ∈ (bld0, bld1) =
          runDB $ insert_ $ BuildSubmission tcId t h BuildPending Nothing Nothing
    | t ∈ (brk0, brk1) = do
          handleBreaks
          handleFixes
    | t ∈ (brk0, fix1) = do
          -- Reject breaks.
          mapM_ (\(_path, name) -> do
              runDB $ insertErrorBreak Nothing name "The break-it round is over."
            ) (addedTests ++ modifiedTests)
          handleFixes
    | t < bld0 = runDB $ insertErrorBuild "The contest has not started."
    | t < brk0 = runDB $ insertErrorBuild "The build deadline has passed."
    -- | t > brk1 = runDB $ insertErrorBreak Nothing "late break" "The contest is over."
    | t > brk1 = runDB $ insert_ $ FixSubmission tcId t h FixRejected Nothing (Just "The contest has ended.") Nothing Nothing
  where
    -- Invalidate outdated break submissions then insert new entries
    -- Added tests are treated the same as modified tests to account for
    -- people deleting then adding them back.
    handleBreaks = do
        gitConfig <- appGitConfig <$> getYesod
        forM_ (addedTests ++ modifiedTests) $ \(path, name) -> do
            -- JP: Do we need to check if name is valid/contains bad characters? What about casing?
            runDB $ do
                oldBreaks <- selectList [BreakSubmissionTeam ==. tcId, BreakSubmissionName ==. name] []
                forM_ oldBreaks $ \(Entity id _) -> do
                    withdrawBreakSubmission id
                    -- deleteWhere [ BreakFixSubmissionBreak ==. id ]
                    -- updateWhere [ BreakFixSubmissionBreak ==. id ]
                    --             -- [ BreakFixSubmissionStatus =. BreakRejected
                    --             [ BreakFixSubmissionResult =. Nothing ] -- JP: Set to BreakFailed, or delete them?
            testCase <- parseBreakMsg pId path h gitConfig
            runDB $ case testCase of
                Just (BreakMsg breakType tId breakJson) -> do
                    -- Check if team exists.
                    resM <- get tId
                    case resM of 
                        Just _ ->
                            insertPendingBreak tId name breakType breakJson
                        Nothing -> 
                            insertErrorBreak (Just tId) name "Invalid target team. Doesn't exist."

                    -- target <- getLatestBuildOrFix tId
                    -- case target of
                    --     Right (_,t) -> insertPendingBreak tId name t breakType
                    --     Left msg    -> insertErrorBreak (Just tId) name "Invalid target team. Doesn't exist."
                Nothing -> insertErrorBreak Nothing name "Invalid JSON in test.json"
    -- Handle each change to `build/` as a fix submission
    handleFixes =  when buildChanged $
        runDB $ insert_ $ FixSubmission tcId t h FixPending Nothing Nothing Nothing Nothing
    (∈) x (lo,hi) = lo <= x && x <= addUTCTime tolerance hi
      where tolerance = 10 * 60
    buildChanged = any ("build" `isInfixOf`) modified
    addedTests = testCases added
    modifiedTests = testCases modified
    testCases ps = [(p,n) | (p,Just n) <- zip ps (map testName ps)]
    testName p = case reverse (splitDirectories p) of
        "test.json":name:"break":_ -> Just (pack name)
        _                           -> Nothing

    insertErrorBuild msg = insert_ $
        BuildSubmission tcId t h BuildPullFail (Just (Textarea msg)) Nothing

    insertErrorBreak tId name msg = do
        insert_ $ BreakSubmission tcId tId t h name BreakPullFail Nothing (Just msg) Nothing Nothing Nothing (Just False) False
        -- insert_ $ BreakFixSubmission id Nothing Nothing Nothing BreakRejected (Just BreakFailed)

    insertPendingBreak tId name breakType breakJson = do
        insert_ $ BreakSubmission tcId (Just tId) t h name BreakPending (Just breakType) Nothing (Just breakJson') Nothing Nothing Nothing False
      
      where
        breakJson' = Text.decodeUtf8With Text.lenientDecode (BSL.toStrict $ J.encode breakJson)

parseBreakMsg :: Int -> String -> Text -> GitConfiguration -> LHandler (Maybe BreakMsg)
-- Parse JSON file from GitLab repo at given path
parseBreakMsg pId fn commitHash (GitlabConfiguration gitConfig) = liftIO $ catchAny (do
    s <- Gitlab.runGitLab gitConfig $ runMaybeT $ do -- JP: Do we need to catch this?
        rf <- MaybeT $ Gitlab.repositoryFiles' pId (pack fn) commitHash
        lift $ Gitlab.repositoryFileBlob pId (Gitlab.blob_id rf) --JP: Why does this return a String?
    return $ s >>= J.decodeStrict . BSC.pack
  ) (\e -> do
    print e
    return Nothing
  )

-- Extracted information from the JSON message
data PushMsg = PushMsg
    { projectId :: Int
    , commits :: [Commit]
    , gitUrl :: Text
    } deriving (Eq,Show)
data Commit = Commit
    { commitHash :: Text
    , commitAdded :: [String] -- `String` for working with `FilePath` lib
    , commitModified :: [String]
    } deriving (Eq,Show)
data Project = Project {
    projectWebUrl :: Text
  }

instance J.FromJSON PushMsg where
    parseJSON = J.withObject "PushEvent" $ \o -> do
        projectId <- o .: "project_id"
        commits <- o .: "commits" -- ) >>= parseJSON
        project <- o .: "project"

        return $ PushMsg projectId commits $ projectWebUrl project

instance J.FromJSON Commit where
    parseJSON = J.withObject "Commit" $ \o ->
        Commit <$> o .: "id"
               <*> o .: "added"
               <*> o .: "modified"

instance J.FromJSON Project where
    parseJSON = J.withObject "Project" $ \o ->
        Project <$> o .: "web_url"

-- Break submission JSON file
data BreakMsg = BreakMsg
    { break_type :: BreakType
    , target_team :: TeamContestId
    , break_json :: J.Value
    } deriving (Eq, Show)
instance J.FromJSON BreakMsg where
    parseJSON = J.withObject "Break message" $ \o ->
        BreakMsg <$> o .: "type"
                 <*> o .: "target_team"
                 <*> pure (J.Object o)
