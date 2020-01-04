{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Problem.Shared where

import Control.Monad.Error as E
import Core (keyToInt)
import Core.SSH
import Data.Aeson (FromJSON(..))
import Data.Aeson ((.=), (.:), (.:?), Value(..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.List as List
-- import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import qualified Database.Esqueleto as E
import Network.SSH.Client.SimpleSSH
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath

import BuildSubmissions
import Common
--import Core.Modular.Class
import Problem.Class

checkSubmissionRound2 :: ContestId -> Entity BreakSubmission -> ErrorT BreakError DatabaseM ()
checkSubmissionRound2 contestId (Entity bsId bs) = checkSubmissionLimit $ 
    if submitteamid == targetteamid then
        reject "Cannot break yourself."
    else do
        targetTeamM <- lift $ runDB $ get targetteamid
        case targetTeamM of 
            Nothing -> do
                reject "Invalid target team. Doesn't exist."
            Just tt | teamContestContest tt /= contestId -> do
                reject "Invalid target team. Not in this contest."
            Just _ -> do
                validBreakTeam <- lift $ runDB $ do
                    bsId <- selectFirst [BuildSubmissionTeam ==. targetteamid] [Desc BuildSubmissionId]
                    case bsId of
                        Just (Entity bsId _) -> do
                            buildSubmissionPassesRequiredTests contestId bsId
                        Nothing ->
                            return False
                if not validBreakTeam then
                    reject "Invalid target team."
                else
                    return ()

    where
        submitteamid = breakSubmissionTeam bs
        targetteamid = breakSubmissionTargetTeam bs

        checkSubmissionLimit cps = do
            let totalLimit = 5
            previousCount <- lift $ runDB $ count $ breakBaseFilters bsId submitteamid targetteamid
            if previousCount >= totalLimit then
                reject "Submission limit against this team reached."
            else do
                cps
                
        reject msg = throwError $ BreakErrorRejected msg
            -- runDB $ update bsId [BreakSubmissionStatus =. BreakRejected, BreakSubmissionMessage =. Just msg]
            -- return False

breakBaseFilters :: BreakSubmissionId -> Key TeamContest -> Key TeamContest -> [Filter BreakSubmission]
breakBaseFilters bsId submitteamid targetteamid = [] -- FIXME [BreakSubmissionStatus !=. BreakPullFail, BreakSubmissionStatus !=. BreakRejected, BreakSubmissionStatus !=. BreakPending, BreakSubmissionTeam ==. submitteamid, BreakSubmissionTargetTeam ==. targetteamid, BreakSubmissionStatus !=. BreakPullFail, BreakSubmissionId !=. bsId, BreakSubmissionResult !=. Just BreakIncorrect]

checkIntegrityLimit :: () => Entity BreakSubmission -> ErrorT BreakError DatabaseM ()
checkIntegrityLimit (Entity bsId bs) = do
                let integrityLimit = 1
                previousCount <- lift $ runDB $ count $ (BreakSubmissionBreakType ==. Just BreakIntegrity):breakBaseFilters bsId (breakSubmissionTeam bs) (breakSubmissionTargetTeam bs)
                when (previousCount >= integrityLimit) $ 
                    throwError $ BreakErrorRejected "You may only submit one integrity attack against a team."

checkConfidentialityLimit :: () => Entity BreakSubmission -> ErrorT BreakError DatabaseM ()
checkConfidentialityLimit (Entity bsId bs) = do
    let limit = 1
    previousCount <- lift $ runDB $ count $ (BreakSubmissionBreakType ==. Just BreakConfidentiality):breakBaseFilters bsId (breakSubmissionTeam bs) (breakSubmissionTargetTeam bs)
    when (previousCount >= limit) $ 
        throwError $ BreakErrorRejected "You may only submit one confidentiality attack against a team."

loadBreakSubmissionJSON :: (ModularBreakTest a, FromJSON a) => BreakSubmissionId -> String -> ErrorT BreakError DatabaseM a
loadBreakSubmissionJSON bsId location = do
    -- Load input json.
    breakJSONE <- safeReadFileLazy location
    case breakJSONE of
        Left _err -> do
            throwError $ BreakErrorRejected "Submission's JSON file not found."
        Right breakJSON -> case Aeson.eitherDecode' breakJSON of
            Left _err -> do
                throwError $ BreakErrorRejected "Invalid JSON in break submission."
            Right breakTest -> do
                storeJSONandUpdateSubmissionType breakJSON breakTest
                return breakTest
                
    where
        storeJSONandUpdateSubmissionType j t = lift $ runDB $ update bsId [BreakSubmissionJson =. Just (Text.decodeUtf8With Text.lenientDecode (BSL.toStrict j)), BreakSubmissionBreakType =. Just (breakTestToType t)]
            
-- | Check that the break 'description.txt' exists. 
checkForBreakDescription :: (MonadIO m) => BreakSubmission -> RunnerOptions -> ErrorT BreakError m ()
checkForBreakDescription submission opts = do
    let team = show $ keyToInt $ breakSubmissionTeam submission
    let breakName = Text.unpack $ breakSubmissionName submission
    let repoDir = runnerRepositoryPath opts
    let loc' = FilePath.joinPath [repoDir, "repos", team, "break", breakName, "description"]
    let loc = FilePath.addExtension loc' "txt"
    exists <- liftIO $ Directory.doesFileExist loc
    when (not exists) $ 
        throwError $ BreakErrorRejected "description.txt not found"

-- Check that the fix 'description.txt' exists.
checkForFixDescription :: (MonadIO m) => FixSubmission -> RunnerOptions -> ErrorT FixError m ()
checkForFixDescription submission opts = do
    let team = show $ keyToInt $ fixSubmissionTeam submission
    let fixName = Text.unpack $ fixSubmissionName submission
    let repoDir = runnerRepositoryPath opts
    let loc' = FilePath.joinPath [repoDir, "repos", team, "fix", fixName, "description"]
    let loc = FilePath.addExtension loc' "txt"
    exists <- liftIO $ Directory.doesFileExist loc
    when (not exists) $
        throwError $ FixErrorRejected "description.txt not found"

data BreakError = 
      BreakErrorSystem String
    | BreakErrorBuildFail BS8.ByteString BS8.ByteString  -- Break marked as failed and output shown to user.
    | BreakErrorRejected String -- Reject break submission.
    | BreakErrorTimeout

data FixError = 
      FixErrorSystem String
    | FixErrorBuildFail BS8.ByteString BS8.ByteString
    | FixErrorRejected String
    | FixErrorTimeout

instance Error FixError where
    strMsg = FixErrorSystem

instance BackendError FixError where
    backendTimeout = FixErrorTimeout

instance Error BreakError where
    strMsg = BreakErrorSystem

instance BackendError BreakError where
    backendTimeout = BreakErrorTimeout

data BuildError = 
      BuildError String -- Error logged. 
    | BuildFail BS8.ByteString BS8.ByteString  -- Build marked as failed and output shown to user.
    | BuildErrorTimeout

instance Error BuildError where
    strMsg = BuildError

instance BackendError BuildError where
    backendTimeout = BuildErrorTimeout

data OracleErr = 
      OracleErr String
    | OracleErrTimeout

instance Error OracleErr where
    strMsg = OracleErr

instance BackendError OracleErr where
    backendTimeout = OracleErrTimeout

buildTestName :: BuildTest -> Text
buildTestName (BuildTestCore (Entity _ (ContestCoreTest{..}))) = contestCoreTestName
buildTestName (BuildTestPerformance (Entity _ ContestPerformanceTest{..})) = contestPerformanceTestName
buildTestName (BuildTestOptional (Entity _ ContestOptionalTest{..})) = contestOptionalTestName

data JSONBreakTest = 
    JSONBreakCorrectnessTest Aeson.Value
  | JSONBreakIntegrityTest Aeson.Value
  | JSONBreakConfidentialityTest Aeson.Value
  | JSONBreakCrashTest Aeson.Value
  | JSONBreakSecurityTest Aeson.Value

instance ModularBreakTest JSONBreakTest where
    breakTestToType (JSONBreakCorrectnessTest _) = BreakCorrectness
    breakTestToType (JSONBreakIntegrityTest _) = BreakIntegrity
    breakTestToType (JSONBreakConfidentialityTest _) = BreakConfidentiality
    breakTestToType (JSONBreakCrashTest _) = BreakCrash
    breakTestToType (JSONBreakSecurityTest _) = BreakSecurity

breakTestTypeToSuccessfulResult :: BreakType -> BreakSubmissionResult
breakTestTypeToSuccessfulResult BreakCorrectness = undefined --FIXME BreakCorrect
breakTestTypeToSuccessfulResult BreakCrash = undefined --FIXME BreakCorrect
breakTestTypeToSuccessfulResult BreakIntegrity = undefined --FIXME BreakExploit
breakTestTypeToSuccessfulResult BreakConfidentiality = undefined --FIXME BreakExploit
breakTestTypeToSuccessfulResult BreakSecurity = undefined --FIXME BreakExploit

breakTestToJSONBreakTest :: (Error r, MonadError r m) => Entity BreakSubmission -> m (JSONBreakTest, BreakSubmission)
breakTestToJSONBreakTest (Entity bsId bs) = do
    constr <- case breakSubmissionBreakType bs of
            Just BreakCorrectness -> return JSONBreakCorrectnessTest
            Just BreakCrash -> return JSONBreakCrashTest
            Just BreakConfidentiality -> return JSONBreakConfidentialityTest
            Just BreakIntegrity -> return JSONBreakIntegrityTest
            Just BreakSecurity -> return JSONBreakSecurityTest
            Nothing ->
                throwError $ strMsg $ "Unknown break type for break submission: " ++ show (keyToInt bsId)
    case breakSubmissionJson bs of
        Nothing -> 
            throwError $ strMsg $ "No JSON stored for break submission: " ++ show (keyToInt bsId)
        Just json -> case Aeson.decodeStrict $ Text.encodeUtf8 json of
            Nothing -> 
                throwError $ strMsg $ "Invalid JSON stored for break submission: " ++ show (keyToInt bsId)
            Just test -> 
                return ( constr test, bs)

instance FromJSON JSONBreakTest where
    parseJSON j@(Aeson.Object o) = do
        (typ :: String) <- o .: "type"
        case typ of
            "correctness" ->
                return $ JSONBreakCorrectnessTest j
            "integrity" ->
                return $ JSONBreakIntegrityTest j
            "confidentiality" ->
                return $ JSONBreakConfidentialityTest j
            "crash" ->
                return $ JSONBreakCrashTest j
            "security" ->
                return $ JSONBreakSecurityTest j
            _ ->
                fail "Not a valid test type."
    parseJSON _ = fail "Not a JSON object."

data BreakResult = BreakResult {
    breakResult :: Maybe Bool
  , breakResultMessage :: Maybe Text
  }

instance FromJSON BreakResult where
    parseJSON (Aeson.Object o) = do
        res <- o .:? "result"
        message <- o .:? "error"
        return $ BreakResult res message

    parseJSON _ = fail "BreakResult is not a JSON object."

data BuildResult = BuildResult {
    buildResult :: Bool
  , buildResultMessage :: Maybe Text
  , buildResultTime :: Maybe Double
  }
  deriving (Show)

instance FromJSON BuildResult where
    parseJSON (Aeson.Object o) = do
        res <- o .: "result"
        message <- o .:? "error"
        time <- o .:? "time"
        return $ BuildResult res message time
    parseJSON _ = fail "BuildResult is not a JSON object."

isBuildTestRequired :: BuildTest -> Bool
isBuildTestRequired (BuildTestCore _) = True
isBuildTestRequired (BuildTestPerformance (Entity _ p)) = not $ contestPerformanceTestOptional p
isBuildTestRequired (BuildTestOptional _) = False

-- `exec` is the filepath of the oracle.
runOracle :: (BackendError e, MonadIO m) => Session -> String -> Aeson.Value -> ErrorT e m (Maybe OracleOutput)
runOracle session exec input = do
    let json = BSL.toStrict $ Aeson.encode $ Aeson.object [
            "type" .= ("oracle" :: String)
          , "input" .= input
          , "target" .= exec
          , "oracle_user" .= oracleUser
          ]

    -- Upload json input.
    uploadString session json destJson

    -- Launch grader.
    (Result resOut' _ _) <- executioner' session testUser grader [destJson]
    putLog "Output received."

    -- Drop newline. 
    let (resOut, _) = BS.breakSubstring "\n" resOut'
    putLog $ show resOut

    -- Parse resOut.
    return $ Aeson.decodeStrict' resOut
    -- output <- ErrorT $ case Aeson.decodeStrict' resOut of
    --     Nothing ->
    --         return $ Left $ strMsg $ "Could not decode oracle output: " <> BS8.unpack resOut
    --     Just r ->
    --         return $ Right r

    -- return output

runTestAt :: (BackendError e, MonadIO m, FromJSON a) => Session -> Text -> ErrorT e m a
runTestAt session location = do
    -- Launch grader.
    (Result resOut' resErr _) <- executioner' session testUser grader [Text.unpack location]
    putLog "Output received."

    -- Drop newline. 
    let (resOut, _) = BS.breakSubstring "\n" resOut'
    putLog $ show resErr
    putLog $ show resOut

    -- Parse resOut.
    output <- ErrorT $ case Aeson.decodeStrict' resOut of
        Nothing ->
            return $ Left $ strMsg $ "Could not decode test output: " <> BS8.unpack resOut
        Just r ->
            return $ Right r

    return output

-- `exec` is the name of the target executable.
runBuildTest :: (BackendError e, MonadIO m) => Session -> String -> (BuildTest, Aeson.Value) -> ErrorT e m (BuildTest, BuildResult)
runBuildTest session exec (test, input) = do
    let json = BSL.toStrict $ Aeson.encode $ Aeson.object [
            "type" .= ("build" :: String)
          , "input" .= input
          , "target" .= (baseDir ++ "/" ++ exec)
          , "client_user" .= clientUser
          ]

    -- putLog $ show json

    -- Upload json input.
    uploadString session json destJson

    res <- runTestAt session $ Text.pack destJson
    return ( test, res)

    where
        baseDir :: String
        baseDir = "/home/builder/submission/build"

-- Constants used in runOracle and runBuildTest.
oracleUser :: String
oracleUser = "client"
clientUser :: String
clientUser = "builder"
testUser :: String
testUser = "ubuntu"
destJson :: String
destJson = "/tmp/inputjson"
grader :: String
grader = "/usr/bin/grader"

-- recordBuildResult :: Key BuildSubmission -> (BuildTest, BuildResult) -> DatabaseM (Either String ())
recordBuildResult submissionId (BuildTestCore (Entity testId _), BuildResult{..}) = do
    lift $ lift $ runDB $ insert_ $ BuildCoreResult submissionId testId buildResult buildResultMessage
    return ()
recordBuildResult submissionId (BuildTestPerformance (Entity testId _), BuildResult False msgM _) = do
    lift $ lift $ runDB $ insert_ $ BuildPerformanceResult submissionId testId Nothing msgM
    return ()
recordBuildResult _ (BuildTestPerformance (Entity _ _), BuildResult True _ Nothing) = 
    fail "Output did not include time"
recordBuildResult submissionId (BuildTestPerformance (Entity testId _), BuildResult True msgM (Just time)) = do
    lift $ lift $ runDB $ insert_ $ BuildPerformanceResult submissionId testId (Just time) msgM
    return ()
recordBuildResult submissionId (BuildTestOptional (Entity testId _), BuildResult{..}) = do
    lift $ lift $ runDB $ insert_ $ BuildOptionalResult submissionId testId buildResult buildResultMessage
    return ()

executioner :: (MonadIO m, BackendError e) => Session -> String -> String -> ByteString -> ErrorT e m Result
executioner session user destProgram args = do
    let destExecArgs = "/tmp/destExecArgs"
    uploadString session args destExecArgs
    executioner' session user destProgram [destExecArgs]

executioner' :: (MonadIO m, BackendError e) => Session -> String -> String -> [String] -> ErrorT e m Result
executioner' session user destProgram args = do
    let encodedArgs = B64.encode $ BSL.toStrict $ Aeson.encode args
    let destArgs = "/tmp/destArgs"
    uploadString session encodedArgs destArgs
    res@(Result out _err _exit) <- runSSH (strMsg "Could not execute command on target.") $ execCommand session $ "sudo -i -u " <> user <> " bash -c 'sudo /usr/bin/executioner " <> destProgram <> " " <> destArgs <> "'"
    case out of
        "thisisatimeoutthisisatimeoutthisisatimeoutthisisatimeout" -> 
            throwError backendTimeout
        "thisisatimeoutthisisatimeoutthisisatimeoutthisisatimeout\n" -> 
            throwError backendTimeout
        _ ->
            return res
        
parseCoreTest :: (Error e, Monad m) => Entity ContestCoreTest -> ErrorT e m (BuildTest, Aeson.Value)
parseCoreTest = parseTestHelper contestCoreTestTestScript BuildTestCore

parsePerformanceTest :: (Error e, Monad m) => Entity ContestPerformanceTest -> ErrorT e m (BuildTest, Aeson.Value)
parsePerformanceTest = parseTestHelper contestPerformanceTestTestScript BuildTestPerformance

parseOptionalTest :: (Monad m, Error e) => Entity ContestOptionalTest -> ErrorT e m (BuildTest, Aeson.Value)
parseOptionalTest = parseTestHelper contestOptionalTestTestScript BuildTestOptional

runJSONBreakTest :: (MonadIO m, BackendError e, FromJSON a) => Session -> String -> String -> JSONBreakTest -> ErrorT e m a
runJSONBreakTest session targetDestFile oracleDestFile breakTest = do
    let input = BSL.toStrict $ Aeson.encode $ Aeson.object [
            "test" .= jsonTest
          , "oracle" .= oracleDestFile
          , "client_user" .= ("builder" :: String)
          , "oracle_user" .= ("breaker" :: String)
          , "target" .= targetDestFile
          , "type" .= testType
          ]

    -- Upload json input.
    uploadString session input destJson

    runTestAt session $ Text.pack destJson

    where
        testType = case breakTest of
          JSONBreakCorrectnessTest _ -> "correctness" :: Text
          JSONBreakIntegrityTest _ -> "integrity"
          JSONBreakConfidentialityTest _ -> "confidentiality"
          JSONBreakCrashTest _ -> "crash"
          JSONBreakSecurityTest _ -> "security"
            
        jsonTest = case breakTest of
          JSONBreakCorrectnessTest v -> v
          JSONBreakIntegrityTest v -> v
          JSONBreakConfidentialityTest v -> v
          JSONBreakCrashTest v -> v
          JSONBreakSecurityTest v -> v
            
verifyAndFilterBreaksForFix breaks' filterF = foldM helper [] breaks'
    where
        helper acc bsE@(Entity bsId bs) = undefined {-FIXME-} {-do
            fixes <- lift $ runDB $ E.select $ E.from $ \(fs `E.InnerJoin` fsb) -> do
                E.on (fs E.^. FixSubmissionId E.==. fsb E.^. FixSubmissionBugsFix)
                E.where_ (fsb E.^. FixSubmissionBugsBugId E.==. E.val bsId E.&&. (fs E.^. FixSubmissionStatus E.!=. E.val FixRejected E.&&. fs E.^. FixSubmissionStatus E.!=. E.val FixBuildFail E.&&. fs E.^. FixSubmissionStatus E.!=. E.val FixInvalidBugId))
                return fsb -- TODO: E.countRows
            when (List.length fixes > 1) $
                throwError $ FixErrorRejected $ "Already fixed break '" <> Text.unpack (breakSubmissionName bs) <> "' (" <> show (keyToInt bsId) <> ")"

            -- Include if passes filter.
            if filterF bs then
                return $ bsE:acc
            else
                return acc -}

archiveLocation :: TeamContestId -> Text -> RunnerOptions -> FilePath
archiveLocation team' hash' opts = 
    let repoDir = runnerRepositoryPath opts in
    let loc'' = FilePath.joinPath [repoDir, "archives", team, hash] in
    let loc' = FilePath.addExtension loc'' "tar" in
    FilePath.addExtension loc' "gz"

    where
        hash = Text.unpack hash'
        team = show $ keyToInt team'

getArchiveLocation :: (MonadIO m, E.Error e) => TeamContestId -> Text -> RunnerOptions -> ErrorT e m FilePath
getArchiveLocation team hash opts = ErrorT $ do
    let loc = archiveLocation team hash opts
    exists <- liftIO $ Directory.doesFileExist loc
    if exists then
        return $ Right loc
    else
        return $ Left $ E.strMsg $ "Archive does not exist: " <> loc

getBuildArchiveLocation :: (MonadIO m, E.Error e) => BuildSubmission -> RunnerOptions -> ErrorT e m FilePath
getBuildArchiveLocation submission opts = 
    let hash = buildSubmissionCommitHash submission in
    let team = buildSubmissionTeam submission in
    getArchiveLocation team hash opts

getBreakArchiveLocation :: (MonadIO m, E.Error e) => BreakSubmission -> RunnerOptions -> ErrorT e m FilePath
getBreakArchiveLocation submission opts = do
    -- Old for art gallery:
    -- let hash = Text.unpack $ breakSubmissionCommitHash submission in
    -- let team = show $ keyToInt $ breakSubmissionTeam submission
    -- getArchiveLocation team hash opts
    let team = show $ keyToInt $ breakSubmissionTeam submission
    let breakName = Text.unpack $ breakSubmissionName submission
    let repoDir = runnerRepositoryPath opts
    let loc' = FilePath.joinPath [repoDir, "breaks", team, breakName]
    let loc = FilePath.addExtension loc' "zip"
    exists <- liftIO $ Directory.doesFileExist loc
    if exists then
        return loc
    else
        throwError $ E.strMsg $ "Archive does not exist: " <> loc

getFixArchiveLocation :: (MonadIO m, E.Error e) => FixSubmission -> RunnerOptions -> ErrorT e m FilePath
getFixArchiveLocation submission opts = 
    let hash = fixSubmissionCommitHash submission in
    let team = fixSubmissionTeam submission in
    getArchiveLocation team hash opts

data OracleOutput = 
      OracleOutputError Text
    | OracleOutputSuccess Text

instance FromJSON OracleOutput where
    parseJSON (Object o) = do
        result <- o .: "result"
        if result then do
            (output' :: Value) <- o .: "output"
            let prettyConf = Aeson.defConfig {Aeson.confIndent = Aeson.Spaces 2}
            let output = Text.decodeUtf8With Text.lenientDecode $ BSL.toStrict $ Aeson.encodePretty' prettyConf output'
            return $ OracleOutputSuccess output
        else do
            err <- o .: "error"
            return $ OracleOutputError err
    parseJSON _ = mzero

-- data BuildError = 
--       BuildErrorInfrastructure String
--     | BuildErrorBuildFailed

parseTestHelper :: (FromJSON i, Monad m, E.Error e) => (t -> Text) -> (Entity t -> a) -> Entity t -> ErrorT e m (a, i)
parseTestHelper getInput constr t@(Entity _ test) = ErrorT $ 
    let input' = getInput test in
    case Aeson.decodeStrict' $ Text.encodeUtf8 input' of
        Nothing ->
            return $ Left $ E.strMsg $ "Invalid test input: " <> (show input')
        Just input ->
            return $ Right $ (constr t, input)

class ModularBreakTest b where
    breakTestToType :: b -> BreakType

teamSubmissionLocation :: RunnerOptions -> TeamContestId -> Text -> FilePath
teamSubmissionLocation opts tcId hash = FilePath.addExtension (FilePath.joinPath pieces) "zip"
  where basePath = runnerRepositoryPath opts
        pieces = [basePath, "commits", show (keyToInt tcId), Text.unpack hash]
