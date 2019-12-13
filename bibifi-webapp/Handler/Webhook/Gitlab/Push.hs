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
    pushTime <- getCurrentTime
    contest <- runDB $ do
        TeamContest _ cId _ _ _ nonce <- get404 tcId
        -- FIXME compare against token
        get404 cId
    PushMsg pId cmts <- requireJsonBody
    mapM_ (handleCommit pushTime pId contest tcId) cmts

handleCommit :: UTCTime -> Int -> Contest -> TeamContestId -> Commit -> LHandler ()
-- Insert a submission of the right round in the database for given team and commit-hash
handleCommit t pId (Contest _ _ bld0 bld1 brk0 brk1) tcId (Commit h added modified)
    | t ∈ (bld0, bld1) =
          runDB $ insert_ $ BuildSubmission tcId t h BuildPending Nothing Nothing
    | t ∈ (brk0, brk1) = do
          -- Invalidate outdated break submissions then insert new entries
          -- Added tests are treated the same as modified tests to account for
          -- people deleting then adding them back.
          forM_ (addedTests ++ modifiedTests) $ \(f, name) -> do
              runDB $ do
                  breaks <- selectList [BreakSubmissionName ==. name] []
                  forM_ breaks $ \(Entity id _) -> do
                      update id [ BreakSubmissionValid =. Just False
                                , BreakSubmissionMessage =. Just "Break resubmitted" ]
                      updateWhere [ BreakFixSubmissionBreak ==. id ]
                                  [ BreakFixSubmissionStatus =. BreakRejected
                                  , BreakFixSubmissionResult =. Nothing ]
              parsed <- parseGitLabJSON J.decode pId f
              case parsed of
                  Just (BreakMsg _ tId) -> runDB $ do
                      -- TODO validate targetTeam
                      target' <- getLatestBuildOrFix tId
                      case target' of
                          Just target -> do
                              id <- insert $ BreakSubmission tcId tId t h name Nothing Nothing Nothing Nothing
                              insert_ $ BreakFixSubmission id target Nothing Nothing BreakPending Nothing
                          Nothing -> do
                              id <- insert $ BreakSubmission tcId tId t h name Nothing (Just "Invalid target team") Nothing (Just False)
                              insert_ $ BreakFixSubmission id Nothing Nothing Nothing BreakRejected (Just BreakFailed)
                  Nothing -> runDB $ do
                      id <- insert $ BreakSubmission tcId (toSqlKey 1) t h name Nothing (Just "invalid JSON in test.json") Nothing (Just False)
                      insert_ $ BreakFixSubmission id Nothing Nothing Nothing BreakRejected (Just BreakFailed)
          -- Handle each change to `build/` as a fix submission
          when buildChanged $ -- TODO: so no explicit fix, no name?
              runDB $ insert_ $ FixSubmission tcId t h FixPending Nothing "" Nothing Nothing Nothing
    | t < bld0 = return () -- TODO: contest not started
    | t < brk0 = return () -- TODO: invalid build
    | t > brk1 = return () -- TODO: invalid break/fix
  where
    (∈) x (lo,hi) = lo <= x && x <= addUTCTime tolerance hi
      where tolerance = 10 * 60
    addedTests    = testNames added
    modifiedTests = testNames modified
    buildChanged = any ("build/" `isInfixOf`) modified
    testNames ps = [(p,n) | (p,Just n) <- zip ps (map testName ps)]
    testName p = case reverse (splitPath p) of
        "test.json":name:"break/":_ -> Just (pack name)
        _                           -> Nothing
    getLatestBuildOrFix tId = do
        latestBuild <- selectFirst [ BuildSubmissionTeam ==. tId ]
                                   [ Desc BuildSubmissionTimestamp ]
        latestFix   <- selectFirst [ FixSubmissionTeam ==. tId
                                   , FixSubmissionResult ==. Just FixFixed]
                                   [ Desc FixSubmissionTimestamp ]
        case (latestBuild, latestFix) of
            (_, Just (Entity id _))                     -> return $ Just $ Just id
            (Just (Entity _ b), _)
                | buildSubmissionStatus b == BuildBuilt -> return $ Just $ Nothing
            _                                           -> return Nothing

parseGitLabJSON :: (BS.ByteString -> Maybe a) -> Int -> String -> LHandler (Maybe a)
-- Parse JSON file from GitLab repo at given path
parseGitLabJSON parse pId fn = undefined {- do
    s <- G.runGitLabConfig config $ do
        Just f <- GF.repositoryFiles' pId fn "master"
        GF.repositoryFileBlob pId (GT.blob_id f)
    let Just msg = parse (BS.fromString s)
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
