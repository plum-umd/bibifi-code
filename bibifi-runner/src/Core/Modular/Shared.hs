{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Core.Modular.Shared where

import Control.Monad.Error
import Core (keyToInt)
import Core.SSH
import Data.Aeson (FromJSON(..))
import Data.Aeson ((.=), (.:), (.:?))
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import Network.SSH.Client.SimpleSSH
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath

import BuildSubmissions
import Common
import Core.Modular.Class

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
breakBaseFilters bsId submitteamid targetteamid = [BreakSubmissionStatus !=. BreakPullFail, BreakSubmissionStatus !=. BreakRejected, BreakSubmissionStatus !=. BreakPending, BreakSubmissionTeam ==. submitteamid, BreakSubmissionTargetTeam ==. targetteamid, BreakSubmissionStatus !=. BreakPullFail, BreakSubmissionId !=. bsId, BreakSubmissionResult !=. Just BreakIncorrect]

checkIntegrityLimit :: () => Entity BreakSubmission -> ErrorT BreakError DatabaseM ()
checkIntegrityLimit (Entity bsId bs) = do
                let integrityLimit = 1
                previousCount <- lift $ runDB $ count $ (BreakSubmissionType ==. Just BreakIntegrity):breakBaseFilters bsId (breakSubmissionTeam bs) (breakSubmissionTargetTeam bs)
                when (previousCount >= integrityLimit) $ 
                    throwError $ BreakErrorRejected "You may only submit one integrity attack against a team."

checkConfidentialityLimit :: () => Entity BreakSubmission -> ErrorT BreakError DatabaseM ()
checkConfidentialityLimit (Entity bsId bs) = do
    let limit = 1
    previousCount <- lift $ runDB $ count $ (BreakSubmissionType ==. Just BreakConfidentiality):breakBaseFilters bsId (breakSubmissionTeam bs) (breakSubmissionTargetTeam bs)
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
        storeJSONandUpdateSubmissionType j t = lift $ runDB $ update bsId [BreakSubmissionJson =. Just (Text.decodeUtf8With Text.lenientDecode (BSL.toStrict j)), BreakSubmissionType =. Just (breakTestToType t)]
            
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

data BuildTest = 
    BuildTestCore (Entity ContestCoreTest)
  | BuildTestPerformance (Entity ContestPerformanceTest)
  | BuildTestOptional (Entity ContestOptionalTest)

data BuildResult = BuildResult {
    buildResult :: Bool
  , buildResultMessage :: Maybe Text
  , buildResultTime :: Maybe Double
  }

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

runBuildTest :: (BackendError e, MonadIO m) => Session -> (BuildTest, Aeson.Value) -> ErrorT e m (BuildTest, BuildResult)
runBuildTest session (test, input) = do
    let json = BSL.toStrict $ Aeson.encode $ Aeson.object [
            "type" .= ("build" :: String)
          , "input" .= input
          , "target" .= (baseDir ++ "/" ++ exec)
          , "client_user" .= clientUser
          ]

    -- Upload json input.
    uploadString session json destJson

    -- Launch grader
    (Result resOut' _ _) <- executioner session testUser grader $ BS8.pack destJson
    putLog "Output received."

    -- Drop newline. 
    let (resOut, _) = BS.breakSubstring "\n" resOut'
    putLog $ show resOut

    -- Parse resOut.
    output <- ErrorT $ case Aeson.decodeStrict' resOut of
        Nothing ->
            return $ Left $ strMsg $ "Could not decode test output: " <> BS8.unpack resOut
        Just r ->
            return $ Right r

    return (test, output)

    where
        exec :: String
        exec = "server" -- Note: Change this depending on the spec.

        clientUser :: String
        clientUser = "builder"
        testUser :: String
        testUser = "ubuntu"
        baseDir :: String
        baseDir = "/home/builder/submission/build"
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

    where

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

