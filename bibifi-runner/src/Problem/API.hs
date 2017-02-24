module Problem.API where

import Control.Monad
import Control.Monad.Error
import Data.Aeson ((.=), FromJSON)
import qualified Data.Aeson as Aeson
import qualified Data.IORef.Lifted as IO
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.List as List
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import Network.SSH.Client.SimpleSSH
import qualified System.Directory as Directory
import System.FilePath ((</>))
import qualified System.FilePath as FilePath
import Yesod.Form.Fields (Textarea(..))

import Cloud
import Common
import Core (keyToInt)
import Core.Score
import Core.SSH
import Problem.Class
import Problem.Shared hiding (grader, runBuildTest, runTestAt)
import Scorer.Class

newtype APIProblem = APIProblem (Entity Contest)

instance ExtractContest APIProblem where
    extractContest (APIProblem c) = c

instance ScorerClass APIProblem where
    scoreContestBuild p _ = defaultScoreBuildRound $ extractContestId p
    scoreContestBreak p _ = defaultScoreBreakRound $ extractContestId p
    scoreContestFix p _ = defaultScoreFixRound $ extractContestId p

instance ProblemRunnerClass APIProblem where
    runOracleSubmission (APIProblem _) opts (Entity submissionId submission) = 
        -- Parse oracle input.
        let inputObjectM = Aeson.decodeStrict' $ Text.encodeUtf8 $ oracleSubmissionInput submission in
        case inputObjectM of
            Nothing -> do
                -- Invalid input so store as error.
                runDB $ update submissionId [OracleSubmissionStatus =. OracleError, OracleSubmissionOutput =. Just "Invalid input"]

                -- Return success.
                return $ Just True
            Just inputObject -> do
                
                -- Start instance.
                let conf = runnerCloudConfiguration opts
                let manager = runnerHttpManager opts
                resultE <- runErrorT $ launchOneInstanceWithTimeout conf manager 60 $ \_inst session -> do

                    -- setupFirewall session

                    -- Send oracle.
                    putLog "Sending oracle files."
                    uploadFolder session (runnerProblemDirectory opts) "/problem"

                    -- Run oracle.
                    runOracle session inputObject

                -- Return result.
                case resultE of
                    Left (OracleErr err) -> do
                        putLog err
                        return (Just False)
                    Left OracleErrTimeout -> do
                        return Nothing
                    Right res -> do
                        let (status, output) = case res of
                              Nothing ->
                                (OracleError, Nothing)
                              Just (OracleOutputError err) ->
                                (OracleError, Just err)
                              Just (OracleOutputSuccess out) ->
                                (OracleFinished, Just out)
                        runDB $ update submissionId [OracleSubmissionOutput =. output, OracleSubmissionStatus =. status]
                        return $ Just True

        where
            runOracle :: (BackendError e, MonadIO m) => Session -> Aeson.Value -> ErrorT e m (Maybe OracleOutput)
            runOracle session input = do
                let json = BSL.toStrict $ Aeson.encode $ Aeson.object [
                        "type" .= ("oracle" :: String)
                      , "input" .= input
                      ]

                -- Upload json input.
                uploadString session json destJson

                -- Launch oracle.
                (Result resOut' _ _) <- executioner' session testUser "/problem/grader" [destJson]

                -- Drop newline.
                let (resOut, _) = BS.breakSubstring "\n" resOut'
                -- putLog $ show resOut

                -- Parse resOut.
                return $ Aeson.decodeStrict' resOut

    runBuildSubmission (APIProblem (Entity contestId _contest)) opts (Entity submissionId submission) = do
        -- Retrieve tests from database.
        coreTests' <- runDB $ selectList [ContestCoreTestContest ==. contestId] []
        performanceTests' <- runDB $ selectList [ContestPerformanceTestContest ==. contestId] []
        optionalTests' <- runDB $ selectList [ContestOptionalTestContest ==. contestId] [Asc ContestOptionalTestName]

        coreDoneRef <- IO.newIORef False
        resultsE <- runErrorT $ do
            -- Parse tests.
            coreTests <- mapM parseCoreTest coreTests'
            performanceTests <- mapM parsePerformanceTest performanceTests'
            optionalTests <- mapM parseOptionalTest optionalTests'

            -- Make sure build submission tar and MITMs exist.
            archiveLocation <- getBuildArchiveLocation submission opts 

            -- Delete any previous stored results.
            lift $ runDB $ deleteWhere [BuildCoreResultSubmission ==. submissionId]
            lift $ runDB $ deleteWhere [BuildPerformanceResultSubmission ==. submissionId]
            lift $ runDB $ deleteWhere [BuildOptionalResultSubmission ==. submissionId]

            -- Run instance.
            let conf = runnerCloudConfiguration opts
            let manager = runnerHttpManager opts
            launchOneInstanceWithTimeout conf manager 60 $ \_inst session -> do
                putLog "Sending build submission."
                let destArchiveLocation = "/home/ubuntu/submission.tar.gz"
                _ <- runSSH (BuildError "Could not send submission") $ sendFile session 0o666 archiveLocation destArchiveLocation

                -- setupFirewall session

                -- Setup directory.
                (Result _ _ exit) <- runSSH (BuildError "Could not make test directory.") $ execCommand session "sudo -i -u builder mkdir /home/builder/submission"
                when (exit /= ExitSuccess) $
                    fail "Could not make test directory."

                -- Extract submission.
                putLog "Extracting build submission."
                (Result _ _ exit) <- runSSH (BuildError "Could not extract submission") $ execCommand session ("cd /home/builder/submission; sudo -u builder tar -xf " <> destArchiveLocation)
                when (exit /= ExitSuccess) $ 
                    fail "Could not extract submission"

                -- Build submission. 
                putLog "Building submission."
                (Result stderr stdout exit) <- runSSH (BuildError "Could not run make") $ execCommand session "sudo -i -u builder make -B -C /home/builder/submission/build"
                when (exit /= ExitSuccess) $
                    throwError $ BuildFail stderr stdout

                -- Map over tests.
                let (requiredTests, optTests) = List.partition (isBuildTestRequired . fst) (coreTests <> performanceTests <> optionalTests)
                portRef <- initialPort
                requiredResults <- mapM (runBuildTest session portRef) requiredTests
                mapM_ (recordBuildResult submissionId) requiredResults

                -- Indicate core tests passed. 
                coreDoneRef `IO.writeIORef` True

                -- Continue running optional tests.
                mapM_ (\test -> do
                    result <- runBuildTest session portRef test 
                    (recordBuildResult submissionId) result
                  ) optTests



        case (resultsE :: Either BuildError ()) of
            Left (BuildFail stdout' stderr') -> do
                let stdout = Just $ Textarea $ Text.decodeUtf8With Text.lenientDecode stdout'
                let stderr = Just $ Textarea $ Text.decodeUtf8With Text.lenientDecode stderr'
                runDB $ update submissionId [BuildSubmissionStatus =. BuildBuildFail, BuildSubmissionStdout =. stdout, BuildSubmissionStderr =. stderr]
                putLog "Build failed"
                return $ Just (True, False)
            Left (BuildError err) -> do
                putLog err
                return $ Just (False, False)
            Left BuildErrorTimeout -> do
                -- Timeout.
                -- Check if core completed.
                coreDone <- IO.readIORef coreDoneRef
                if coreDone then 
                    buildBuilt
                else
                    return Nothing
            Right () ->
                buildBuilt
        
        where
            buildBuilt = do
                runDB $ update submissionId [BuildSubmissionStatus =. BuildBuilt]
                return $ Just (True, True)

    runBreakSubmission (APIProblem (Entity contestId _contest)) opts bsE@(Entity submissionId submission) = do
        resultE <- runErrorT $ do
            checkSubmissionRound2 contestId bsE

            (breakTest :: JSONBreakTest) <- loadBreakSubmissionJSON submissionId breakJSONFile

            -- Make sure break submission exists.
            checkForBreakDescription submission opts

            -- Start instance.
            let conf = runnerCloudConfiguration opts
            let manager = runnerHttpManager opts
            launchOneInstanceWithTimeout conf manager 30 $ \_inst session -> do
                -- Setup firewall.
                -- setupFirewall session

                -- Upload problem files.
                putLog "Sending problem files."
                uploadFolder session (runnerProblemDirectory opts) "/problem"

                -- Upload break folder.
                uploadFolder session breakDir "/break"

                -- Upload target.
                putLog "Sending target submission."
                let targetArchiveLocation = FilePath.addExtension (FilePath.joinPath [basePath,"round2",targetTeamIdS]) "zip"
                let destTargetArchiveLocation = "/home/ubuntu/submission.zip"
                _ <- runSSH (BreakErrorSystem "Could not send target submission") $ sendFile session 0o666 targetArchiveLocation destTargetArchiveLocation

                -- Extract target.
                putLog "Extracting target submission."
                -- putLog $ ("cd /home/builder; sudo -u builder unzip " <> destTargetArchiveLocation <> "; mv ./repos/" <> targetTeamIdS <> "/ ./submission")
                (Result _ _ exit) <- runSSH (BreakErrorSystem "Could not extract submission") $ execCommand session ("cd /home/builder; sudo -u builder unzip " <> destTargetArchiveLocation <> "; sudo -u builder mv ./" <> targetTeamIdS <> " ./submission")
                when (exit /= ExitSuccess) $ 
                    fail "Could not extract target submission"

                -- Build target.
                putLog "Building target submission."
                (Result _ _ exit) <- runSSH (BreakErrorSystem "Building target failed") $ execCommand session $ "sudo -i -u builder make -B -C /home/builder/submission/build"
                when (exit /= ExitSuccess) $
                    fail "Building target failed"

                portRef <- initialPort
                res <- runBreakTest session portRef breakTest
                return (res, breakTest)


        -- Record result.
        case resultE of
            Left (BreakErrorSystem err) -> do
                systemFail err
            Left BreakErrorTimeout ->
                return Nothing
            Left (BreakErrorBuildFail stdout' stderr') -> do
                let stdout = Just $ Textarea $ Text.decodeUtf8With Text.lenientDecode stdout'
                let stderr = Just $ Textarea $ Text.decodeUtf8With Text.lenientDecode stderr'
                runDB $ update submissionId [BreakSubmissionStatus =. BreakRejected, BreakSubmissionResult =. Nothing, BreakSubmissionMessage =. Just "Running make failed", BreakSubmissionStdout =. stdout, BreakSubmissionStderr =. stderr]
                userFail "Build failed"
            Left (BreakErrorRejected msg) -> do
                runDB $ update submissionId [BreakSubmissionStatus =. BreakRejected, BreakSubmissionResult =. Nothing, BreakSubmissionMessage =. Just msg, BreakSubmissionStdout =. Nothing, BreakSubmissionStderr =. Nothing]
                userFail msg
            Right (BreakResult (Just False) msgM, _) -> do
                runDB $ update submissionId [BreakSubmissionStatus =. BreakRejected, BreakSubmissionResult =. Nothing, BreakSubmissionMessage =. fmap Text.unpack msgM, BreakSubmissionStdout =. Nothing, BreakSubmissionStderr =. Nothing]
                userFail $ maybe "Test failed" Text.unpack msgM
            Right (BreakResult Nothing _, _) -> do
                runDB $ update submissionId [BreakSubmissionStatus =. BreakJudging, BreakSubmissionMessage =. Nothing]
                return $ Just ( True, False)
            Right (BreakResult (Just True) _, breakTest) -> do
                let result = breakTestTypeToSuccessfulResult $ breakTestToType breakTest
                runDB $ update submissionId [BreakSubmissionStatus =. BreakTested, BreakSubmissionResult =. Just result, BreakSubmissionMessage =. Nothing]
                return $ Just ( True, True)


        where
            basePath = runnerRepositoryPath opts
            breakJSONFile = FilePath.addExtension (FilePath.joinPath [basePath, "repos", submitTeamIdS, "break", breakName, "test"]) "json"
            breakDir = FilePath.joinPath [basePath, "repos", submitTeamIdS, "break", breakName]
            targetTeamIdS = show $ keyToInt $ breakSubmissionTargetTeam submission
            submitTeamIdS = show $ keyToInt $ breakSubmissionTeam submission
            breakName = Text.unpack $ breakSubmissionName submission

            userFail err = do
                putLog err
                return $ Just (True, False)
            systemFail err = do
                putLog err
                return $ Just (False, False)

    runFixSubmission = undefined

-- Could use a state transformer...
initialPort :: MonadIO m => m (IO.IORef Int)
initialPort = liftIO $ IO.newIORef 6300

getNextPort :: MonadIO m => IO.IORef Int -> m Int
getNextPort portRef = liftIO $ do
    port <- IO.readIORef portRef
    IO.writeIORef portRef (port + 20)
    return port

runBreakTest session portRef breakTest = do
    port <- getNextPort portRef
    let json = BSL.toStrict $ Aeson.encode $ Aeson.object [
            "test" .= jsonTest
          , "port" .= port
          , "type" .= ("break" :: String)
          , "classification" .= testType
          ]

    -- Upload json input.
    uploadString session json destJson

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
            

runBuildTest session portRef (test, input) = do
    port <- getNextPort portRef
    let json = BSL.toStrict $ Aeson.encode $ Aeson.object [
            "type" .= ("build" :: String)
          , "port" .= port
          , "input" .= input
          ]

    -- Upload json input.
    uploadString session json destJson

    res <- runTestAt session $ Text.pack destJson

    return ( test, res)

runTestAt :: (BackendError e, MonadIO m, FromJSON a) => Session -> Text -> ErrorT e m a
runTestAt session location = do
    -- Launch grader.
    (Result resOut' resErr _) <- executioner' session testUser "/problem/grader" [Text.unpack location]
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


uploadFolder session localDir destDir' = do
    -- Check destDir.
    when ( FilePath.isRelative destDir || destDir == "/") $ 
        throwError $ strMsg $ "Destination directory is not absolute: " <> destDir

    -- TODO: Delete dest directory first XXX

    -- Make target directory.
    (Result _ _ exit) <- runSSH (strMsg ("Could not make directory `" <> destDir <> "`.")) $ 
        execCommand session ( "sudo -i mkdir -p " <> destDir)
    when (exit /= ExitSuccess) $
        fail ( "Could not make directory `" <> destDir <> "`.")

    -- Update permissions.
    (Result _ _ exit) <- runSSH (strMsg ( "Could not update directory `" <> destDir <> "` permissions.")) $ 
        execCommand session ( "sudo -i chmod -R 0700 " <> destDir)
    when (exit /= ExitSuccess) $
        fail ( "Could not update directory `" <> destDir <> "` permissions.")

    -- Get contents.
    contents' <- liftIO $ listDirectory localDir
    let contents = fmap (\f -> (localDir </> f, destDir </> f)) contents'

    mapM_ (\( local, dest) -> do
        -- Check if it's a file or directory.
        isFile <- liftIO $ Directory.doesFileExist local  
        if isFile then do
        
            -- Upload file.
            _ <- runSSH (strMsg $ "Could not send file: " ++ local) $ sendFile session 0o777 local dest
            return ()

        else

            -- Recursively upload directory.
            uploadFolder session local dest
      
      ) contents
    
    where
        destDir = FilePath.normalise destDir'

        listDirectory path = filter f <$> Directory.getDirectoryContents path
            where f filename = filename /= "." && filename /= ".."

