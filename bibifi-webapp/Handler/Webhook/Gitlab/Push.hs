module Handler.Webhook.Gitlab.Push (postWebhookGitlabPushR) where

import PostDependencyType
    ( BuildSubmissionStatus(..)
    , BreakSubmissionStatus(..)
    , FixSubmissionStatus(..)
    )
import Import as I
import Data.Aeson as J
import Data.List (isInfixOf)
import qualified Data.ByteString.Lazy as BS
import System.FilePath.Posix as F (splitPath)
import Data.Text as T (pack)
--import GitLab as G
--import GitLab.Types as GT
--import GitLab.API.Projects as GP

postWebhookGitlabPushR :: TeamContestId -> Text -> Handler ()
-- Handles Gitlab push notification.
-- There's no way to authenticate that the call is from Gitlab at the moment..
-- We only treat the call as a ping, and check back with Gitlab to retrieve information
-- regarding the commit hash.
postWebhookGitlabPushR tcId token = runLHandler $ do
    -- FIXME: check that token matches, and using constant time comparison
    pushTime <- getCurrentTime
    contest <- runDB $ do
        TeamContest _ cId _ _ _ _ <- get404 tcId
        get404 cId
    PushMsg pId cmts <- requireJsonBody
    mapM_ (handleCommit pushTime pId contest tcId) cmts

handleCommit :: UTCTime -> Int -> Contest -> TeamContestId -> Commit -> LHandler ()
-- Insert a submission of the right round in the database for given team and commit-hash
handleCommit t pId (Contest _ _ bld0 bld1 brk0 brk1) tcId (Commit h added modified)
    | t ∈ (bld0, bld1) =
          insertDB_ $ BuildSubmission tcId t h BuildPending Nothing Nothing
    | t ∈ (brk0, brk1) = do
          -- Invalidate outdated break submissions then insert new entries
          -- Added tests are treated the same as modified tests to account for
          -- people deleting then adding them back.
          forM_ (addedTests ++ modifiedTests) $ \(f, name) -> do
              runDB $ do
                  breaks <- selectList [BreakSubmissionName ==. pack name] []
                  forM_ breaks $ \(Entity id _) -> do
                      update id [ BreakSubmissionValid =. Just False
                                , BreakSubmissionMessage =. Just "Break resubmitted" ]
                      updateWhere [ BreakFixSubmissionBreak ==. id ]
                                  [ BreakFixSubmissionStatus =. BreakRejected
                                  , BreakFixSubmissionResult =. Nothing ]
              BreakMsg _ tId <- parseGitLabJSON J.decode pId f
              insertDB_ $ BreakSubmission tcId tId t h (pack name) Nothing Nothing Nothing Nothing
          -- Handle each change to `build/` as a fix submission
          when buildChanged $ -- TODO: so no explicit fix, no name?
              insertDB_ $ FixSubmission tcId t h FixPending Nothing "" Nothing Nothing Nothing
    | t < bld0 = return () -- TODO: contest not started
    | t < brk0 = return () -- TODO: invalid build
    | t > brk1 = return () -- TODO: invalid break/fix
  where
    (∈) x (lo,hi) = lo <= x && x <= addUTCTime tolerance hi
      where tolerance = 10 * 60
    insertDB_ s = runDB (insert s) >> return ()
    addedTests    = testNames added
    modifiedTests = testNames modified
    buildChanged = any ("build/" `isInfixOf`) modified
    testNames ps = [(p,n) | (p,Just n) <- zip ps (map testName ps)]
    testName p = case reverse (splitPath p) of
        "test.json":name:"break/":_ -> Just name
        _                           -> Nothing

parseGitLabJSON :: (BS.ByteString -> Maybe a) -> Int -> String -> LHandler a
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
