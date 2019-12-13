module Handler.Webhook.Gitlab.Push (postWebhookGitlabPushR) where

import PostDependencyType
    ( BuildSubmissionStatus(..)
    , BreakSubmissionStatus(..)
    , FixSubmissionStatus(..)
    , BreakSubmissionResult(..)
    , FixSubmissionResult(..)
    )
import Import as I
import Data.Aeson as J
import Data.List (isInfixOf)
import qualified Data.ByteString.Lazy as BS
import System.FilePath.Posix as F (splitPath)
import Data.Text as T (pack)
import Database.Persist.Sql (toSqlKey)
--import GitLab as G
--import GitLab.Types as GT
--import GitLab.API.Projects as GP

postWebhookGitlabPushR :: TeamContestId -> Text -> Handler ()
-- Handles Gitlab push notification.
-- There's no way to authenticate that the call is from Gitlab at the moment..
-- We only treat the call as a ping, and check back with Gitlab to retrieve information
-- regarding the commit hash.
postWebhookGitlabPushR tcId token = runLHandler $ do
    (cId, nonce) <- runDB $ do
        TeamContest _ cId _ _ _ nonce <- get404 tcId
        return (cId, nonce)
    unless (token === nonce) $
        permissionDenied "Unmatched token"
    pushTime <- getCurrentTime
    contest <- runDB $ get404 cId
    PushMsg pId cmts <- requireJsonBody
    mapM_ (handleCommit pushTime pId contest tcId) cmts
  where
    (===) s1 s2 = undefined -- TODO: constant time

handleCommit :: UTCTime -> Int -> Contest -> TeamContestId -> Commit -> LHandler ()
-- Insert submission of the right kind (build/break/fix) into the database based on push time
handleCommit t pId (Contest _ _ bld0 bld1 brk0 brk1) tcId (Commit h added modified)
    | t ∈ (bld0, bld1) =
          runDB $ insert_ $ BuildSubmission tcId t h BuildPending Nothing Nothing
    | t ∈ (brk0, brk1) = do
          -- Invalidate outdated break submissions then insert new entries
          -- Added tests are treated the same as modified tests to account for
          -- people deleting then adding them back.
          forM_ (testCases added ++ testCases modified) $ \(path, name) -> do
              runDB $ do
                  oldBreaks <- selectList [BreakSubmissionName ==. name] []
                  forM_ oldBreaks $ \(Entity id _) -> do
                      update id [ BreakSubmissionValid =. Just False
                                , BreakSubmissionMessage =. Just "Break resubmitted" ]
                      updateWhere [ BreakFixSubmissionBreak ==. id ]
                                  [ BreakFixSubmissionStatus =. BreakRejected
                                  , BreakFixSubmissionResult =. Nothing ]
              testCase <- parseBreakMsg pId path
              runDB $ case testCase of
                  Just (BreakMsg _ tId) | tId /= tcId -> do
                      target <- getLatestBuildOrFix tId
                      case target of
                          Right t  -> insertPendingBreak tId name t
                          Left msg -> insertErrorBreak tId name msg
                  Just _ -> insertErrorBreak tcId name "self targeting"
                  Nothing -> insertErrorBreak (toSqlKey 1) name "invalid JSON in test.json"
          -- Handle each change to `build/` as a fix submission
          when buildChanged $
              runDB $ insert_ $ FixSubmission tcId t h FixPending Nothing "" Nothing Nothing Nothing
    | t < bld0 = runDB $ insertErrorBuild "contest not started"
    | t < brk0 = runDB $ insertErrorBuild "build deadline passed"
    | t > brk1 = runDB $ insertErrorBreak (toSqlKey 1) "late break" "contest over"
  where
    (∈) x (lo,hi) = lo <= x && x <= addUTCTime tolerance hi
      where tolerance = 10 * 60
    buildChanged = any ("build/" `isInfixOf`) modified
    testCases ps = [(p,n) | (p,Just n) <- zip ps (map testName ps)]
    testName p = case reverse (splitPath p) of
        "test.json":name:"break/":_ -> Just (pack name)
        _                           -> Nothing
    insertErrorBuild msg = insert_ $
        BuildSubmission tcId t h BuildBuildFail (Just (Textarea msg)) Nothing
    insertErrorBreak tId name msg = do
        id <- insert $ BreakSubmission tcId tId t h name Nothing (Just msg) Nothing (Just False)
        insert_ $ BreakFixSubmission id Nothing Nothing Nothing BreakRejected (Just BreakFailed)
    insertPendingBreak tId name target = do
        id <- insert $ BreakSubmission tcId tId t h name Nothing Nothing Nothing Nothing
        insert_ $ BreakFixSubmission id target Nothing Nothing BreakPending Nothing
    getLatestBuildOrFix tId = do
        latestBuild <- selectFirst [ BuildSubmissionTeam ==. tId ]
                                   [ Desc BuildSubmissionTimestamp ]
        latestFix   <- selectFirst [ FixSubmissionTeam ==. tId
                                   , FixSubmissionResult ==. Just FixFixed]
                                   [ Desc FixSubmissionTimestamp ]
        case (latestBuild, latestFix) of
            (_, Just (Entity id _))                     -> return $ Right $ Just id
            (Just (Entity _ b), _) -- TODO: should filter be above, or we insist that the latest build be valid?
                | buildSubmissionStatus b == BuildBuilt -> return $ Right $ Nothing
            _                                           -> return $ Left "No valid target"

parseBreakMsg :: Int -> String -> LHandler (Maybe BreakMsg)
-- Parse JSON file from GitLab repo at given path
parseBreakMsg pId fn = undefined {- do
    s <- G.runGitLabConfig config $ do
        Just f <- GF.repositoryFiles' pId fn "master"
        GF.repositoryFileBlob pId (GT.blob_id f)
    let Just msg = J.decode (BS.fromString s)
    return msg -}

-- Extracted information from the JSON message
data PushMsg = PushMsg
    { projectId :: Int
    , commits :: [Commit]
    } deriving (Eq,Show)
data Commit = Commit
    { commitHash :: Text
    , commitAdded :: [String] -- `String` for working with `FilePath` lib
    , commitModified :: [String]
    } deriving (Eq,Show)

instance J.FromJSON PushMsg where
    parseJSON = J.withObject "PushEvent" $ \o ->
        PushMsg <$> o .: "project_id"
                <*> ((o .: "commits") >>= parseJSON)
instance J.FromJSON Commit where
    parseJSON = J.withObject "Commit" $ \o ->
        Commit <$> o .: "id"
               <*> o .: "added"
               <*> o .: "modified"

-- Break submission JSON file
data BreakMsg = BreakMsg
    { break_type :: Text
    , target_team :: TeamContestId
    } deriving (Eq, Show)
instance J.FromJSON BreakMsg where
    parseJSON = J.withObject "Break message" $ \o ->
        BreakMsg <$> o .: "type"
                 <*> o .: "target_team"
