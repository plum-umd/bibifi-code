{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Core.Modular.ArtGallery where

import Core (keyToInt)
import Core.SSH hiding (uploadString)
import Control.Monad.Error
import Data.Aeson (FromJSON(..),(.:),(.:?),(.!=),(.=),Value(..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Char as Char
import qualified Data.IORef.Lifted as IO
import qualified Data.List as List
import Data.Monoid
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Vector as Vector
import qualified Database.Esqueleto as E
import Network.SSH.Client.SimpleSSH
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import qualified Text.Read as Text

import Cloud
import Cloud.EC2
import Common
import Core.Modular.Class
import Core.Score

newtype ArtGallery = ArtGallery (Entity Contest)

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' = foldr

instance ModularContest ArtGallery where
    scoreContestBuild (ArtGallery (Entity cId _)) _ = defaultScoreBuildRound cId
    scoreContestBreak (ArtGallery (Entity cId _)) _ = defaultScoreBreakRound cId
    scoreContestFix (ArtGallery (Entity cId _)) _ = defaultScoreFixRound cId
    runOracleSubmission (ArtGallery _contest) opts (Entity submissionId submission) = 
        -- Parse input.
        let inputM = Aeson.decodeStrict' $ Text.encodeUtf8 $ oracleSubmissionInput submission in
        case inputM of
            Nothing -> do
                -- Invalid input so store as error.
                runDB $ update submissionId [OracleSubmissionStatus =. OracleError, OracleSubmissionOutput =. Just "Invalid input"]

                -- Return success.
                return $ Just True
            Just inputs -> 
                -- Validate inputs.
                case foldr' validateOracleInput (Right []) inputs of
                    Left err -> do
                        runDB $ update submissionId [OracleSubmissionStatus =. OracleError, OracleSubmissionOutput =. Just err]
                
                        -- Return success.
                        return $ Just True
                    Right inputs ->
                        -- Start EC2.
                        let ec2 = runnerEC2Configuration opts in
                        let manager = runnerHttpManager opts in
                        launchOneEC2WithTimeout ec2 manager 60 ( \e -> do
                                putLog e
                                return False
                            ) $ \_inst session -> do
                                resultE <- runErrorT $ do
                                    -- Block outgoing.
                                    setupFirewall session

                                    -- Send oracle.
                                    putLog "Sending oracle files."
                                    let oracleAppend = runnerOracleDirectory opts ++ "logappend"
                                    let oracleDestAppend = oracleDestDirectory ++ "logappend"
                                    let oracleRead = runnerOracleDirectory opts ++ "logread"
                                    let oracleDestRead = oracleDestDirectory ++ "logread"
                                    _ <- runSSH "Could not send logappend oracle to instance." $ sendFile session 0o777 oracleAppend oracleDestAppend
                                    _ <- runSSH "Could not send logread oracle to instance." $ sendFile session 0o777 oracleRead oracleDestRead

                                    -- Run inputs.
                                    putLog "Running oracle inputs."
                                    flip mapM inputs $ \input' -> do
                                        let input = B64.encode $ Text.encodeUtf8 input'
                                        _ <- runSSH "Could not upload oracle input." $ execCommand session $ "sudo -u client echo " <> (BS8.unpack input) <> " | base64 -d > input.sh"
                                        (Result stdOut' _ exit) <- runSSH "Could not run oracle input." $ execCommand session "sudo -u client sh ./input.sh"
                                        output' <- case exit of
                                              ExitSuccess -> return ["exit" .= (0 :: Integer)]
                                              ExitFailure i -> return ["exit" .= i]
                                              ExitSignal _ -> do
                                                putLog "Oracle output with signal."
                                                return []
                                        let stdOutput = Text.decodeUtf8With lenientDecode stdOut'
                                        return $ Aeson.object $ ("output" .= stdOutput):output'

                                -- Return result.
                                case resultE of
                                    Left err -> do
                                        putLog err
                                        return False
                                    Right out' -> do
                                        -- Store outputs.
                                        putLog "Recording oracle result."
                                        let out = Text.decodeUtf8With lenientDecode $ BSL.toStrict $  Aeson.encodePretty out'
                                        lift $ lift $ runDB $ update submissionId [OracleSubmissionOutput =. Just out, OracleSubmissionStatus =. OracleFinished]

                                        return True
        
        where
            oracleDestDirectory :: String
            oracleDestDirectory = "/home/ubuntu/"

            validateOracleInput :: Text -> Either Text [Text] -> Either Text [Text]
            validateOracleInput _input acc@(Left _) = acc
            validateOracleInput input (Right acc) = 
                -- Must start with logread or logappend.
                if Text.take 10 input /= "logappend " && Text.take 8 input /= "logread " then
                    Left "Inputs must start with logread or logappend"
                -- Must not contain bad characters.
                else if not $ Text.all checkChar input then
                    Left "Invalid character in input"
                else
                    Right $ ("cd /home/client; /home/ubuntu/" <> input):acc

            checkChar c = foldr (\e acc -> acc && c /= e) True ['&','|','\\', '\'', ';'] 

    runBuildSubmission (ArtGallery (Entity contestId _contest)) opts (Entity submissionId submission) = do
        -- Retrieve tests from database.
        coreTests' <- runDB $ selectList [ContestCoreTestContest ==. contestId] []
        performanceTests' <- runDB $ selectList [ContestPerformanceTestContest ==. contestId] []
        optionalTests'' <- runDB $ selectList [ContestOptionalTestContest ==. contestId] []

        -- TODO: This feels so hacky...
        coreDoneRef <- IO.newIORef False
        resultsE <- runErrorT $ do
            -- Parse tests.
            coreTests <- mapM parseCoreTest coreTests'
            performanceTests <- mapM parsePerformanceTest performanceTests'
            optionalTests' <- mapM parseOptionalTest optionalTests''

            -- Make sure build submission tar exists.
            archiveLocation <- getBuildArchiveLocation submission opts 

            -- Start EC2. 
            let ec2 = runnerEC2Configuration opts
            let manager = runnerHttpManager opts
            launchOneEC2WithTimeout' ec2 manager (4 * 60) (return . Left) $ \_inst session -> do
                -- Block outgoing.
                setupFirewall session

                -- Send submission.
                putLog "Sending build submission."
                let destArchiveLocation = "/home/ubuntu/submission.tar.gz"
                _ <- runSSH "Could not send submission" $ sendFile session 0o666 archiveLocation destArchiveLocation

                -- Setup directory.
                (Result _ _ exit) <- runSSH "Could not make test directory." $ execCommand session "sudo -i -u builder mkdir /home/builder/test"
                when (exit /= ExitSuccess) $
                    fail "Could not make test directory."

                -- Extract submission.
                putLog "Extracting build submission."
                -- putLog ("cd /home/builder/test; sudo -u builder tar -xf " <> destArchiveLocation)
                (Result _ _ exit) <- runSSH "Could not extract submission" $ execCommand session ("cd /home/builder/test; sudo -u builder tar -xf " <> destArchiveLocation)
                when (exit /= ExitSuccess) $ 
                    fail "Could not extract submission"

                -- Build submission. 
                putLog "Building submission."
                (Result _ _ exit) <- runSSH "_build_fail" $ execCommand session "sudo -i -u builder make -B -C /home/builder/test/build"
                when (exit /= ExitSuccess) $
                    fail "_build_fail"

                -- Map over tests.
                let (requiredTests, optionalTests) = List.partition isTestRequired (coreTests <> performanceTests <> optionalTests')
                requiredResults <- mapM (runTest submissionId session) requiredTests
                lift $ lift $ lift $ runDB $ mapM_ recorder requiredResults

                -- Indicate core tests passed. 
                coreDoneRef `IO.writeIORef` True

                -- Continue running optional tests. 
                mapM_ (\test -> do
                    result <- runTest submissionId session test
                    lift $ lift $ lift $ runDB $ recorder result
                  ) optionalTests

        -- Compare and record results
        case resultsE of
            -- TODO: Better error datatype?
            Left "_build_fail" -> do
                runDB $ update submissionId [BuildSubmissionStatus =. BuildBuildFail]
                putLog "Build failed"
                return $ Just (True, False)
            Left err -> do
                putLog err
                return $ Just (False, False)
            Right Nothing -> do
                -- Timeout.
                -- Check if core completed.
                coreDone <- IO.readIORef coreDoneRef
                if coreDone then do
                    runDB $ update submissionId [BuildSubmissionStatus =. BuildBuilt]
                    return $ Just (True, True)
                else
                    return Nothing
            Right (Just ()) -> do
                runDB $ update submissionId [BuildSubmissionStatus =. BuildBuilt]
                return $ Just (True, True)

        where
            generateTestScript = generateTestScript' "build/"

            runTest submissionId session (BuildCoreTest testE testIOs batchM) = do
                -- Create new user.
                (user, userT) <- createNewUser' session testE "core"

                -- Generate script.
                let scriptB64 = generateTestScript userT testIOs

                -- Upload script.
                uploadTestScript session submissionId scriptB64

                -- Maybe upload batch script.
                maybeUploadBatchScript session user submissionId batchM

                -- Run script.
                _ <- runSSH "Could not run test script" $ execCommand session $ "sudo bash /home/ubuntu/script.sh"

                -- Parse results. 
                timeM <- parseTestResults user session testIOs

                case timeM of
                    Left err ->
                        -- Return failed.
                        return $ BuildCoreTestResult $ BuildCoreResult submissionId (entityKey testE) False (Just $ Text.pack err)
                    Right _ ->
                        -- Return success.
                        return $ BuildCoreTestResult $ BuildCoreResult submissionId (entityKey testE) True Nothing

            runTest submissionId session (BuildPerformanceTest testE testIOs batchM performanceType) = do
                -- Create new user.
                (user, userT) <- createNewUser' session testE "performance"

                -- Generate script.
                let scriptB64 = generateTestScript userT testIOs

                -- Upload script.
                uploadTestScript session submissionId scriptB64

                -- Maybe upload batch script.
                maybeUploadBatchScript session user submissionId batchM

                -- Run script.
                _ <- runSSH "Could not run test script" $ execCommand session $ "sudo bash /home/ubuntu/script.sh"

                -- Parse results. 
                timeM <- parseTestResults user session testIOs

                -- Compute results.
                case timeM of
                    Left err ->
                        -- Return failed.
                        return $ BuildPerformanceTestResult $ BuildPerformanceResult submissionId (entityKey testE) Nothing (Just $ Text.pack err)
                    Right time -> do
                        -- Get the appropriate metric. 
                        metricE <- case performanceType of
                            PerformanceTime -> 
                                return $ Right time
                            PerformanceSpace -> do
                                logName <- case List.reverse testIOs of
                                    h:_ ->
                                        case List.reverse $ Text.words $ buildTestInput h of
                                            log:_ -> 
                                                return $ Text.unpack log
                                            _ ->
                                                fail "Could not get log name from test"
                                    _ ->
                                        fail "Could not get log name"

                                (Result out _ exit) <- runSSH "Could not retrieve space used" $ execCommand session $ "sudo du -b /home/" <> user <> "/" <> logName <> " | awk '{print $1}'"
                                if exit /= ExitSuccess || out == "" then
                                    return $ Left $ BuildPerformanceResult submissionId (entityKey testE) Nothing $ Just "Log file not found."
                                else case Text.readMaybe $ BS8.unpack out of
                                    Nothing -> 
                                        fail $ "Could not parse space used: " <> BS8.unpack out <> " - " <> logName
                                    Just space ->
                                        return $ Right space

                        case metricE of
                            Left failRes -> 
                                -- Return failure. 
                                return $ BuildPerformanceTestResult failRes
                            Right metric ->
                                -- Return success. 
                                return $ BuildPerformanceTestResult $ BuildPerformanceResult submissionId (entityKey testE) (Just metric) Nothing

            runTest submissionId session (BuildOptionalTest testE testIOs batchM) = do
                -- Create new user.
                (user, userT) <- createNewUser' session testE "optional"

                -- Generate script.
                let scriptB64 = generateTestScript userT testIOs

                -- Upload script.
                uploadTestScript session submissionId scriptB64

                -- Maybe upload batch script.
                maybeUploadBatchScript session user submissionId batchM

                -- Run script.
                _ <- runSSH "Could not run test script" $ execCommand session $ "sudo bash /home/ubuntu/script.sh"

                -- Parse results. 
                timeM <- parseTestResults user session testIOs

                case timeM of 
                    Left err ->
                        -- Return Failed.
                        return $ BuildOptionalTestResult $ BuildOptionalResult submissionId (entityKey testE) False (Just $ Text.pack err)
                    Right _ ->
                        -- Return success.
                        return $ BuildOptionalTestResult $ BuildOptionalResult submissionId (entityKey testE) True Nothing

            isTestRequired (BuildCoreTest _ _ _) = True
            isTestRequired (BuildPerformanceTest (Entity _ test) _ _ _) = not $ contestPerformanceTestOptional test
            isTestRequired (BuildOptionalTest _ _ _) = False

            recorder (BuildCoreTestResult test) = recorder' test
            recorder (BuildPerformanceTestResult test) = recorder' test
            recorder (BuildOptionalTestResult test) = recorder' test
            recorder' test = do
                success <- insertUnique test
                case success of
                    Nothing -> do
                        -- TODO: Should we delete the old one??
                        let err = "Could not record result: " <> show test
                        liftIO $ putStrLn err
                        return $ Left err
                    Just _ ->
                        return $ Right ()

    runBreakSubmission (ArtGallery (Entity _contestId _contest)) opts (Entity submissionId submission) = do
        -- Load input json. 
        breakJSONE <- safeReadFileLazy breakFilePath
        case breakJSONE of
            Left err -> do
                update' BreakRejected Nothing $ Just "Submission's JSON file not found."
                fmap Just $ userFail err -- "Submission's JSON file not found."
            Right breakJSON -> case Aeson.eitherDecode' breakJSON of
                Left err -> do
                    update' BreakRejected Nothing $ Just "Invalid JSON in break submission."
                    fmap Just $ userFail err
                Right breakTest -> do
                    -- Update submission type. TODO: Move this to translator/periodic.
                    updateBreakSubmissionType breakTest

                    runBreakTest breakTest

        where
            userFail err = do
                putLog err
                return (True, False)
            systemFail err = do
                putLog err
                return (False, False)

            oracleBasePath = runnerOracleDirectory opts
            basePath = runnerRepositoryPath opts
            submitTeamIdS = show $ keyToInt $ breakSubmissionTeam submission
            breakName = Text.unpack $ breakSubmissionName submission
            breakFilePath = FilePath.addExtension (FilePath.joinPath [basePath, "repos", submitTeamIdS, "break", breakName]) "json"
            targetTeamIdS = show $ keyToInt $ breakSubmissionTargetTeam submission

            updateBreakSubmissionType (BreakTestCorrectness _ _) = 
                runDB $ update submissionId [BreakSubmissionType =. Just BreakCorrectness]
            updateBreakSubmissionType (BreakTestCrash _ _) = 
                runDB $ update submissionId [BreakSubmissionType =. Just BreakCrash]
            updateBreakSubmissionType (BreakTestIntegrity _ _ _) = 
                runDB $ update submissionId [BreakSubmissionType =. Just BreakIntegrity]
            updateBreakSubmissionType (BreakTestConfidentiality _ _) = 
                runDB $ update submissionId [BreakSubmissionType =. Just BreakConfidentiality]

            update' status resultM msgM = 
                runDB $ update submissionId [BreakSubmissionStatus =. status, BreakSubmissionResult =. resultM, BreakSubmissionMessage =. msgM]

            destArchiveLocation = "/home/ubuntu/submission.zip"

            getLogFiles basePath logFile = breakErrorRejectT (Just "Invalid log file.") $ do
                putLog "Reading log files."
                files' <- liftIO $ Directory.getDirectoryContents basePath
                let files = List.filter (List.isPrefixOf logFile) files'
                when (List.length files == 0) $ 
                    fail "Invalid log file."
                mapM (\name -> do
                        contents <- ErrorT $ safeReadFileLazy $ FilePath.joinPath [basePath,name]
                        return (name, BSL.toStrict contents)
                    ) files

            uploadLogFile session destBase uniq user (name, contents) = do
                let uniqueL = uniq <> show (keyToInt submissionId)
                let destL' = "/tmp/log"
                let destL = FilePath.joinPath [destBase,name]
                uploadString' session uniqueL contents destL'
                sudoMove session destL' destL user

            uploadTarget session = do
                putLog "Sending target submission."
                let targetSubmissionTar = FilePath.addExtension (FilePath.joinPath [basePath,"round2/teamdirs",targetTeamIdS]) "zip"
                _ <- runSSH ("Could not send submission: " <> targetSubmissionTar) $ sendFile session 0o666 targetSubmissionTar destArchiveLocation
                return ()

            extractSubmission session = do
                putLog "Extracting target submission."
                (Result _ _ exit) <- runSSH "Could not extract submission" $ execCommand session ("cd /home/builder; sudo -u builder unzip " <> destArchiveLocation)
                when (exit /= ExitSuccess) $ 
                    fail "Could not extract target submission"

            buildTarget session = do
                putLog "Building target submission."
                (Result _ _ exit) <- runSSH "Build failed" $ execCommand session $ "sudo -i -u builder make -B -C /home/builder/" <> targetTeamIdS <> "/code/build"
                when (exit /= ExitSuccess) $
                    fail "Build failed"

            executioner session user destProgram destArgs = do
                res@(Result out _err _exit) <- breakErrorSystemT $ runSSH "Could not run test on target." $ execCommand session $ "sudo -i -u " <> user <> " bash -c '/usr/bin/executioner " <> destProgram <> " " <> destArgs <> "'"
                case out of
                    "thisisatimeoutthisisatimeoutthisisatimeoutthisisatimeout" -> 
                        throwError $ BreakErrorReject "Command timed out"
                    _ ->
                        return res

            sanitizeTokenArgs args = 
                when (List.any (\arg -> "-K" == map Char.toUpper arg) args) $
                    throwError $ BreakErrorReject "Invalid argument: -K"

            runBreakTest :: BreakTest -> DatabaseM (Maybe (Bool, Bool))
            runBreakTest (BreakTestCorrectness commands batchM) =
                -- Start EC2. 
                let ec2 = runnerEC2Configuration opts in
                let manager = runnerHttpManager opts in
                launchOneEC2WithTimeout ec2 manager 60 systemFail $ \_inst session -> do
                    resultE <- runErrorT $ do
                        -- Setup firewall. 
                        breakErrorSystemT $ setupFirewall session

                        -- Upload oracle.
                        putLog "Sending oracle."
                        let oracleAppend = FilePath.joinPath [oracleBasePath,"logappend"]
                        let oracleDestAppend = FilePath.joinPath [oracleDestDirectory,"logappend"]
                        let oracleRead = FilePath.joinPath [oracleBasePath,"logread"]
                        let oracleDestRead = FilePath.joinPath [oracleDestDirectory,"logread"]
                        _ <- breakErrorSystemT $ runSSH "Could not send logappend oracle to instance." $ sendFile session 0o777 oracleAppend oracleDestAppend
                        _ <- breakErrorSystemT $ runSSH "Could not send logread oracle to instance." $ sendFile session 0o777 oracleRead oracleDestRead

                        -- Upload target.
                        breakErrorSystemT $ uploadTarget session

                        -- Extract submission.
                        breakErrorSystemT $ extractSubmission session

                        -- Build submission. 
                        breakErrorSystemT $ buildTarget session

                        -- Maybe upload batch for oracle.
                        breakErrorSystemT $ maybeUploadBatch session submissionId batchM "/home/server/batch" "server"

                        -- Maybe upload batch for log. 
                        breakErrorSystemT $ maybeUploadBatch session submissionId batchM "/home/client/batch" "client"

                        -- Fold over commands.
                        foldM (runCommands session) (Left "No commands given") commands

                    -- Record results
                    case resultE of
                        Left (BreakErrorSystem err) ->
                            systemFail err
                        Left (BreakErrorReject msg) -> do
                            -- lift $ lift $ update' BreakTested (Just BreakIncorrect) $ Just msg
                            lift $ lift $ update' BreakRejected Nothing $ Just msg
                            userFail msg
                        Right (Left msg) -> do
                            lift $ lift $ update' BreakRejected Nothing $ Just msg
                            userFail msg
                        Right (Right ()) -> do
                            lift $ lift $ update' BreakTested (Just BreakCorrect) Nothing
                            return (True, True)

              where
                oracleDestDirectory :: String
                oracleDestDirectory = "/home/ubuntu/"

                runCommands _ (Right ()) _ = return $ Right ()
                runCommands session (Left _) (BreakTestCommand program args _) = do
                    -- Upload test.
                    let uniqueFilename = "_correctness_" <> show (keyToInt submissionId)
                    let encodedArgs = Text.decodeUtf8 $ B64.encode $ BSL.toStrict $ Aeson.encode args
                    let destArgs = "/tmp/correctness_args"
                    breakErrorSystemT $ uploadString session uniqueFilename encodedArgs destArgs

                    -- Run test on oracle.
                    let oracleProgram = FilePath.joinPath [oracleDestDirectory, program]
                    (Result oracleOut _oracleErr oracleExit) <- executioner session "server" oracleProgram destArgs
                    
                    -- breakErrorSystemT $ runSSH "Could not run test on oracle." $ execCommand session $ "sudo -i -u server bash -c '/usr/bin/executioner " <> oracleProgram <> " " <> destArgs <> "'"

                    -- Run test on target.
                    let targetProgram = FilePath.joinPath ["/home/builder", targetTeamIdS, "code/build", program]
                    (Result out _err exit) <- executioner session "client" targetProgram destArgs
                    
                    -- breakErrorSystemT $ runSSH "Could not run test on target." $ execCommand session $ "sudo -i -u client bash -c '/usr/bin/executioner " <> targetProgram <> " " <> destArgs <> "'"

                    _ <- error "TODO: Check that it isn't an optional feature XXX"

                    -- Compare outputs. 
                    if testCompare' oracleOut out && oracleExit == exit then -- testCompare' oracleErr err && 
                        return $ Left "Target's output matched oracle's output."
                    else
                        return $ Right ()

            runBreakTest (BreakTestCrash _commands _batchM) = do
                -- Judgement required for crashes. 
                update' BreakJudging Nothing Nothing

                -- Return success and no need to rescore. 
                return $ Just (True, False)

            runBreakTest (BreakTestIntegrity [BreakTestCommand program args' _] logFile (BreakTestReplacements replacement)) = do
                resultE <- runErrorT $ do
                    -- Check that it's logread.
                    when (program /= "logread") $
                        throwError $ BreakErrorReject "Program must be logread"
                    -- Check limit.
                    let teamId = breakSubmissionTeam submission
                    let targetId = breakSubmissionTargetTeam submission
                    attackC <- lift $ runDB $ count [BreakSubmissionTeam ==. teamId, BreakSubmissionTargetTeam ==. targetId, BreakSubmissionStatus !=. BreakPullFail, BreakSubmissionStatus !=. BreakRejected, BreakSubmissionStatus !=. BreakPending, BreakSubmissionResult !=. Just BreakIncorrect, BreakSubmissionType ==. Just BreakIntegrity]
                    when (attackC >= 2) $
                        throwError $ BreakErrorReject "You may only submit one integrity attack against a team."
                    
                    -- Check that log file name is alphanumeric. 
                    when (not $ List.all Char.isAlphaNum logFile) $
                        throwError $ BreakErrorReject "Invalid log file."
                        
                    -- Read log files. 
                    let logBase = FilePath.joinPath [basePath, "round2/teamdirs",targetTeamIdS,"integ"]
                    logFiles <- getLogFiles logBase logFile

                    -- logContents <- breakErrorRejectT (Just "Invalid log file.") $ 
                    --     fmap BSL.toStrict $ ErrorT $ safeReadFileLazy logF

                    -- Read log file token. 
                    let logF = FilePath.joinPath [basePath,"round2",targetTeamIdS,"integ",logFile]
                    let tokenF = FilePath.addExtension logF ".secret"
                    token <- breakErrorRejectT (Just "Invalid log file.") $ do
                        contents <- ErrorT $ safeReadFileLazy tokenF
                        let contestsT = Text.decodeUtf8With lenientDecode $ BSL.toStrict contents
                        return $ Text.unpack $ Text.filter Char.isAlphaNum contestsT

                    -- Sanitize args.
                    sanitizeTokenArgs args'

                    -- Add token and filename to args. 
                    let args = ["-K",token] ++ args' ++ [logFile]

                    -- Launch EC2
                    let ec2 = runnerEC2Configuration opts
                    let manager = runnerHttpManager opts
                    launchOneEC2WithTimeout' ec2 manager 60 (return . Left . BreakErrorSystem) $ \_inst session -> do
                        -- Setup firewall. 
                        breakErrorSystemT $ setupFirewall session

                        -- Upload log files.
                        breakErrorSystemT $ mapM_ (uploadLogFile session "/home/server" "_integ_" "server") logFiles

                        -- Upload replacement files.
                        replacementLogs <- mapM (\(name', contents') -> do
                                let name = maybe logFile Text.unpack name'
                                let contents = B64.decodeLenient $ Text.encodeUtf8 contents'
                                when (List.any (== '/') name) $ 
                                    throwError $ BreakErrorReject "Invalid replacement log file name."
                                return (name,contents)
                              ) replacement
                        breakErrorSystemT $ mapM_ (uploadLogFile session "/home/client" "_integ_r_" "client") replacementLogs

                        -- let uniqueR = "_integ_r_" <> show (keyToInt submissionId)
                        -- let destR' = "/tmp/client_log"
                        -- let destR = "/home/client/log"
                        -- let replacementContents = B64.decodeLenient $ Text.encodeUtf8 replacement
                        -- breakErrorSystemT $ uploadString' session uniqueR replacementContents destR'

                        -- -- Move log file.
                        -- breakErrorSystemT $ sudoMove session destR' destR

                        -- Upload target.
                        breakErrorSystemT $ uploadTarget session

                        -- Extract submission.
                        breakErrorSystemT $ extractSubmission session

                        -- Build submission. 
                        breakErrorSystemT $ buildTarget session

                        -- Upload test.
                        let uniqueA = "_integ_args_" <> show (keyToInt submissionId)
                        let encodedArgs = Text.decodeUtf8 $ B64.encode $ BSL.toStrict $ Aeson.encode args
                        let destArgs = "/tmp/args"
                        breakErrorSystemT $ uploadString session uniqueA encodedArgs destArgs

                        -- Run on log file.
                        (Result logOut _ logExit) <- executioner session "server" ("/home/builder/" <> targetTeamIdS <> "/code/build/logread") destArgs

                        -- breakErrorSystemT $ runSSH "Could not run input on target." $ execCommand session $ "sudo -i -u server bash -c '/usr/bin/executioner /home/builder/" <> targetTeamIdS <> "/code/build/logread " <> destArgs <> "'"

                        -- Make sure exit's 0 and does not print "invalid". 
                        when (logExit /= ExitSuccess || testCompare' logOut "" || testCompare' logOut "invalid" || testCompare' logOut "integrity violation") $ do
                            throwError $ BreakErrorReject "Invoking command with unmodified log printed 'integrity violation' or did not exit 0."

                        -- Run on replacement log file.
                        (Result out _ exit) <- executioner session "client" ("/home/builder/" <> targetTeamIdS <> "/code/build/logread") destArgs
                        
                        -- breakErrorSystemT $ runSSH "Could not run modified input on target." $ execCommand session $ "sudo -i -u client bash -c '/usr/bin/executioner /home/builder/" <> targetTeamIdS<> "/code/build/logread " <> destArgs <> "'"

                        -- Check if exit 0 and does not print "invalid" and does not equal log file output. 
                        if exit == ExitSuccess && not (testCompare' out "") && not (testCompare' out "invalid") && not (testCompare' out "integrity violation") && not (testCompare' out logOut) then
                            return ()
                        else
                            -- throwError $ BreakErrorIncorrect
                            throwError $ BreakErrorReject "Invoking command with modified log did not exit 0 or printed 'integrity violation' or did not differ in output."
                
                case resultE of
                    Left (BreakErrorReject msg) -> do
                        update' BreakRejected Nothing $ Just msg
                        fmap Just $ userFail msg
                    Left (BreakErrorSystem err) ->
                        fmap Just $ systemFail err
                    -- Left (BreakErrorIncorrect msg) -> do
                    --     update' BreakTested (Just BreakIncorrect) $ Just msg
                    --     fmap Just $ userFail msg
                    Right Nothing ->
                        -- Timeout.
                        return Nothing
                    Right (Just ()) -> do
                        update' BreakTested (Just BreakExploit) Nothing
                        return $ Just (True, True)
                        
            runBreakTest (BreakTestIntegrity _ _ _) = do
                -- More than one command provided.
                update' BreakRejected Nothing $ Just "Only one command must provided."

                -- Done and no need to rescore. 
                return $ Just (True, False)

            runBreakTest (BreakTestConfidentiality [BreakTestCommand _ _ Nothing] _) = do
                update' BreakRejected Nothing $ Just "No command output given."

                -- Done and no need to rescore. 
                return $ Just (True, False)

            runBreakTest (BreakTestConfidentiality [BreakTestCommand program args' (Just guessedOut)] logFile) = do
                resultE <- runErrorT $ do
                    -- Check that it's logread.
                    when (program /= "logread") $
                        throwError $ BreakErrorReject "Program must be logread"

                    -- Check that guessed output is not 'invalid'.
                    when (testCompare guessedOut "invalid" || testCompare guessedOut "integrity violation") $ 
                        throwError $ BreakErrorReject "'integrity violation' is not an accepted output"

                    -- Check limit.
                    let teamId = breakSubmissionTeam submission
                    let targetId = breakSubmissionTargetTeam submission
                    attackC <- lift $ runDB $ count [BreakSubmissionTeam ==. teamId, BreakSubmissionTargetTeam ==. targetId, BreakSubmissionStatus !=. BreakPullFail, BreakSubmissionStatus !=. BreakRejected, BreakSubmissionStatus !=. BreakPending, BreakSubmissionResult !=. Just BreakIncorrect, BreakSubmissionType ==. Just BreakConfidentiality]
                    when (attackC >= 2) $
                        throwError $ BreakErrorReject "You may only submit one confidentiality attack against a team."

                    -- Check that log file name is alphanumeric. 
                    when (not $ List.all Char.isAlphaNum logFile) $
                        throwError $ BreakErrorReject "Invalid log file."
                        
                    -- Read log files. 
                    let logBase = FilePath.joinPath [basePath, "round2/teamdirs",targetTeamIdS,"conf"]
                    logFiles <- getLogFiles logBase logFile

                    -- logContents <- breakErrorRejectT (Just "Invalid log file.") $ 
                    --     fmap BSL.toStrict $ ErrorT $ safeReadFileLazy logF

                    -- Read log file token. 
                    let logF = FilePath.joinPath [basePath,"round2",targetTeamIdS,"conf",logFile]
                    let tokenF = FilePath.addExtension logF ".secret"
                    token <- breakErrorRejectT (Just "Invalid log file.") $ do
                        contents <- ErrorT $ safeReadFileLazy tokenF
                        let contestsT = Text.decodeUtf8With lenientDecode $ BSL.toStrict contents
                        return $ Text.unpack $ Text.filter Char.isAlphaNum contestsT

                    -- Sanitize args.
                    sanitizeTokenArgs args'

                    -- Add token and filename to args. 
                    let args = ["-K",token] ++ args' ++ [logFile]

                    -- Launch EC2
                    let ec2 = runnerEC2Configuration opts
                    let manager = runnerHttpManager opts
                    launchOneEC2WithTimeout' ec2 manager 60 (return . Left . BreakErrorSystem) $ \_inst session -> do
                        -- Setup firewall. 
                        breakErrorSystemT $ setupFirewall session

                        -- Upload log files.
                        breakErrorSystemT $ mapM_ (uploadLogFile session "/home/server" "_conf_" "server") logFiles

                        -- Upload target.
                        breakErrorSystemT $ uploadTarget session

                        -- Extract submission.
                        breakErrorSystemT $ extractSubmission session

                        -- Build submission. 
                        breakErrorSystemT $ buildTarget session

                        -- Upload test.
                        let uniqueA = "_conf_args_" <> show (keyToInt submissionId)
                        let encodedArgs = Text.decodeUtf8 $ B64.encode $ BSL.toStrict $ Aeson.encode args
                        let destArgs = "/tmp/conf_args"
                        breakErrorSystemT $ uploadString session uniqueA encodedArgs destArgs

                        -- Run args.
                        res@(Result out _ exit) <- executioner session "server" ("/home/builder/" <> targetTeamIdS <> "/code/build/logread") destArgs
                        
                        -- breakErrorSystemT $ runSSH "Could not run input on target." $ execCommand session $ "sudo -i -u server bash -c '/usr/bin/executioner /home/builder/" <> targetTeamIdS <> "/code/build/logread " <> destArgs <> "'"

                        -- Check that out /= 'invalid' and did not exit 0. 
                        when (testCompare' out "" || testCompare' out "invalid" || testCompare' out "integrity violation" || exit /= ExitSuccess) $ do
                            putLog $ show args
                            putLog $ show res
                            throwError $ BreakErrorReject "Running logread returned 'integrity violation', had zero-length output, or did not exit 0."

                        -- Check that out == guessOut. 
                        if testCompare' out (Text.encodeUtf8 guessedOut) then
                            return ()
                        else
                            throwError $ BreakErrorReject "Guessed output did not match."

                case resultE of
                    Left (BreakErrorReject msg) -> do
                        update' BreakRejected Nothing $ Just msg
                        fmap Just $ userFail msg
                    Left (BreakErrorSystem err) ->
                        fmap Just $ systemFail err
                    Right Nothing ->
                        -- Timeout.
                        return Nothing
                    Right (Just ()) -> do
                        update' BreakTested (Just BreakExploit) Nothing
                        return $ Just (True, True)

            runBreakTest (BreakTestConfidentiality _ _) = do
                -- More than one command provided.
                update' BreakRejected Nothing $ Just "Only one command must provided."

                -- Done and no need to rescore. 
                return $ Just (True, False)

            breakErrorRejectT = breakErrorT BreakErrorReject
            breakErrorSystemT = breakErrorT BreakErrorSystem Nothing
            breakErrorT const msgM e = ErrorT $ do
                resE <- runErrorT e
                case (resE, msgM) of
                    (Right r,_) ->
                        return $ Right r
                    (Left _, Just err) -> 
                        return $ Left $ const err
                    (Left err, Nothing) -> 
                        return $ Left $ const err

    runFixSubmission (ArtGallery (Entity contestId _contest)) opts (Entity fixId fix) = do
        -- TODO: Read and parse json.
        --      Handle case for invalid breaks?

        -- Retrieve tests from database.
        coreTests' <- runDB $ selectList [ContestCoreTestContest ==. contestId] []

        -- Retrieve breaks from database.
        breaks'' <- runDB $ E.select $ E.from $ \(fsb `E.InnerJoin` bs) -> do
            E.on (fsb E.^. FixSubmissionBugsBugId E.==. bs E.^. BreakSubmissionId)
            E.where_ (fsb E.^. FixSubmissionBugsFix E.==. E.val fixId)
            return bs
        
        resultE <- runErrorT $ do
            -- Verify breaks fixed and filter them. 
            breaks' <- foldM verifyAndFilterBreaks [] breaks''

            -- Load breaks from JSON. 
            breaks <- mapM (loadBreak opts) breaks'

            -- Parse core tests.
            coreTests <- fixErrorSystemT $ mapM parseCoreTest coreTests'
            
            -- Get fix tar.
            archiveLocation <- fixErrorSystemT $ getFixArchiveLocation fix opts

            -- Start EC2. 
            let ec2 = runnerEC2Configuration opts
            let manager = runnerHttpManager opts
            launchOneEC2WithTimeout' ec2 manager 60 (return . Left . FixErrorSystem) $ \_inst session -> do
                -- Block outgoing.
                fixErrorSystemT $ setupFirewall session

                -- Upload oracle.
                putLog "Sending oracle."
                let oracleAppend = FilePath.joinPath [oracleBasePath,"logappend"]
                let oracleDestAppend = FilePath.joinPath [oracleDestDirectory,"logappend"]
                let oracleRead = FilePath.joinPath [oracleBasePath,"logread"]
                let oracleDestRead = FilePath.joinPath [oracleDestDirectory,"logread"]
                _ <- fixErrorSystemT $ runSSH "Could not send logappend oracle to instance." $ sendFile session 0o777 oracleAppend oracleDestAppend
                _ <- fixErrorSystemT $ runSSH "Could not send logread oracle to instance." $ sendFile session 0o777 oracleRead oracleDestRead

                -- Send submission.
                putLog "Sending fix submission."
                let destArchiveLocation = "/home/ubuntu/submission.tar.gz"
                _ <- fixErrorSystemT $ runSSH "Could not send submission" $ sendFile session 0o666 archiveLocation destArchiveLocation

                -- Setup directory.
                (Result _ _ exit) <- fixErrorSystemT $ runSSH "Could not make test directory." $ execCommand session "sudo -i -u builder mkdir /home/builder/test"
                when (exit /= ExitSuccess) $
                    throwError $ FixErrorSystem "Could not make test directory."

                -- Extract submission.
                putLog "Extracting build submission."
                (Result _ _ exit) <- fixErrorSystemT $ runSSH "Could not extract submission" $ execCommand session ("cd /home/builder/test; sudo -u builder tar -xf " <> destArchiveLocation)
                when (exit /= ExitSuccess) $
                    throwError $ FixErrorSystem "Could not extract submission."

                -- Build submission. 
                putLog "Building submission."
                (Result _ _ exit) <- fixErrorT (const FixErrorBuildFail) Nothing $ runSSH "build failed" $ execCommand session "sudo -i -u builder make -B -C /home/builder/test/fix/code"
                when (exit /= ExitSuccess) $
                    throwError FixErrorBuildFail

                -- Map over correctness tests.
                mapM_ (runTest session fixId) coreTests

                -- Run break tests.
                mapM_ (runBreakTest session fixId) breaks

        -- Record results
        case resultE of
            Left (FixErrorSystem err) -> do
                -- Testing fix failed so mark as pending.
                putLog err
                return $ Just (False, False)
            Left (FixErrorReject msg) -> do
                -- User error so reject fix. 
                update' FixRejected $ Just msg
                putLog msg
                return $ Just (True, False)
            Left FixErrorBuildFail -> do
                -- Building fix submission failed.
                update' FixBuildFail Nothing
                return $ Just (True, False)
            Right Nothing -> do
                -- Timeout.
                return Nothing
            Right (Just ()) -> do
                -- All tests passed so mark for judgement. 
                update' FixJudging Nothing
                return $ Just (True, True)

        where
            oracleDestDirectory :: String
            oracleDestDirectory = "/home/ubuntu/"

            oracleBasePath = runnerOracleDirectory opts

            update' status msgM = 
                runDB $ update fixId [FixSubmissionStatus =. status, FixSubmissionMessage =. msgM]

            fixErrorSystemT :: Monad m => ErrorT String m b -> ErrorT FixError m b
            fixErrorSystemT = fixErrorT FixErrorSystem Nothing

            fixErrorT :: Monad m => (String -> e) -> Maybe String -> ErrorT String m b -> ErrorT e m b
            fixErrorT constr msgM e = ErrorT $ do
                resE <- runErrorT e
                case (resE, msgM) of
                    (Right r,_) ->
                        return $ Right r
                    (Left _, Just err) -> 
                        return $ Left $ constr err
                    (Left err, Nothing) -> 
                        return $ Left $ constr err

            loadBreak :: (MonadIO m) => RunnerOptions -> Entity BreakSubmission -> ErrorT FixError m (Entity BreakSubmission, BreakTest)
            loadBreak opts bsE@(Entity bsId bs) = do
                let breakIdS = show $ keyToInt $ bsId
                let breakNameS = Text.unpack $ breakSubmissionName bs
                let targetTeamIdS = show $ keyToInt $ breakSubmissionTargetTeam bs
                -- ~/backend/round3/<targetteam>/<breakid>_<breakname>.json
                let breakFilePath = FilePath.addExtension (FilePath.joinPath [runnerRepositoryPath opts, "round3", targetTeamIdS, breakIdS <> "_" <> breakNameS]) "json"
                -- Load input json. 
                breakJSONE <- safeReadFileLazy breakFilePath
                case breakJSONE of
                    Left err ->
                        throwError $ FixErrorSystem err
                    Right breakJSON -> case Aeson.eitherDecode' breakJSON of
                        Left err -> 
                            throwError $ FixErrorSystem $ "Could not parse json: " <> err
                        Right breakTest -> do
                            return (bsE,breakTest)


            -- verifyAndFilterBreaks :: [Entity BreakSubmission] -> Entity BreakSubmission -> ErrorT FixError DatabaseM [Entity BreakSubmission]
            verifyAndFilterBreaks acc bsE@(Entity bsId bs) = do
                -- Check that this break is only being fixed once.
                fixes <- lift $ runDB $ E.select $ E.from $ \(fs `E.InnerJoin` fsb) -> do
                    E.on (fs E.^. FixSubmissionId E.==. fsb E.^. FixSubmissionBugsFix)
                    E.where_ (fsb E.^. FixSubmissionBugsBugId E.==. E.val bsId E.&&. (fs E.^. FixSubmissionStatus E.!=. E.val FixRejected E.&&. fs E.^. FixSubmissionStatus E.!=. E.val FixBuildFail E.&&. fs E.^. FixSubmissionStatus E.!=. E.val FixInvalidBugId))
                    return fsb -- TODO: E.countRows
                when (List.length fixes > 1) $
                    throwError $ FixErrorReject $ "Already fixed break '" <> Text.unpack (breakSubmissionName bs) <> "' (" <> show (keyToInt bsId) <> ")"

                -- Include break if it's a correctness violation.
                if breakSubmissionType bs == Just BreakCorrectness then
                    return $ bsE:acc
                else
                    return acc

            runBreakTest session fixId (bsE, BreakTestCorrectness commands batchM) = runBreakTest' session fixId bsE commands batchM
            runBreakTest session fixId (bsE, BreakTestCrash commands batchM) = runBreakTest' session fixId bsE commands batchM
            runBreakTest _ _ ((Entity bsId _),_) = 
                throwError $ FixErrorSystem $ "Invalid break: " <> show (keyToInt bsId)

            runBreakTest' session fixId bsE commands batchM = do
                -- Create new user. 
                (oracleUser, _) <- fixErrorSystemT $ createNewUser' session bsE $ "breakoracle" <> (show $ keyToInt fixId)
                (user, _) <- fixErrorSystemT $ createNewUser' session bsE $ "break" <> (show $ keyToInt fixId)

                -- Maybe upload batch for oracle.
                fixErrorSystemT $ maybeUploadBatch session fixId batchM ("/home/" <> oracleUser <> "/batch") oracleUser

                -- Maybe upload batch for log. 
                fixErrorSystemT $ maybeUploadBatch session fixId batchM ("/home/" <> user <> "/batch") user

                -- Run the commands.
                mapM (runCommands session fixId bsE oracleUser user) commands

            runCommands session fixId (Entity bsId bs) oracleUser user (BreakTestCommand program args _) = do
                    -- Upload test.
                    let uniqueFilename = "_correctness_" <> show (keyToInt fixId)
                    let encodedArgs = Text.decodeUtf8 $ B64.encode $ BSL.toStrict $ Aeson.encode args
                    let destArgs = "/tmp/correctness_args"
                    fixErrorSystemT $ uploadString session uniqueFilename encodedArgs destArgs

                    -- Run test on oracle.
                    let oracleProgram = FilePath.joinPath [oracleDestDirectory, program]
                    (Result oracleOut _oracleErr oracleExit) <- executioner session oracleUser oracleProgram destArgs

                    -- Run test on target.
                    let targetProgram = FilePath.joinPath ["/home/builder/test/fix/code", program]
                    (Result out _err exit) <- executioner session user targetProgram destArgs

                    -- Compare outputs. 
                    if testCompare' oracleOut out && oracleExit == exit then -- testCompare' oracleErr err && 
                        return ()
                    else
                        throwError $ FixErrorReject $ "Fix failed break submission '" <> Text.unpack (breakSubmissionName bs) <> "' (" <> show (keyToInt bsId) <> "). Expected(" <> printRet oracleExit <> "): '" <> printOut oracleOut <> "'. Got(" <> printRet exit <> "): '" <> printOut out <> "'."

            printRet ExitSuccess = "0"
            printRet (ExitFailure ret) = show ret
            printRet (ExitSignal _) = "signal"

            printOut = show . Text.decodeUtf8With Text.lenientDecode

            executioner session user destProgram destArgs = do
                res@(Result out _err _exit) <- fixErrorSystemT $ runSSH "Could not run test on target." $ execCommand session $ "sudo -i -u " <> user <> " bash -c '/usr/bin/executioner " <> destProgram <> " " <> destArgs <> "'"
                case out of
                    "thisisatimeoutthisisatimeoutthisisatimeoutthisisatimeout" -> 
                        throwError $ FixErrorReject "Command timed out"
                    _ ->
                        return res

            runTest session fixId (BuildCoreTest testE@(Entity _ test) testIOs batchM) = do
                -- Create new user.
                (user, userT) <- fixErrorSystemT $ createNewUser' session testE "core"

                -- Generate script.
                let scriptB64 = generateTestScript' "fix/code/" userT testIOs

                -- Upload script.
                fixErrorSystemT $ uploadTestScript session fixId scriptB64

                -- Maybe upload batch script
                fixErrorSystemT $ maybeUploadBatchScript session user fixId batchM

                -- Run script.
                _ <- fixErrorSystemT $ runSSH "Could not run test script" $ execCommand session $ "sudo bash /home/ubuntu/script.sh"

                -- Parse Results.
                timeM <- fixErrorSystemT $ parseTestResults user session testIOs

                case timeM of
                    Left err ->
                        throwError $ FixErrorReject $ "Failed core test '" <> Text.unpack (contestCoreTestName test) <> "'. " <> err
                    Right _ ->
                        return ()
            
            runTest _ _ _ = 
                throwError $ FixErrorSystem "Invalid fix type."

data BuildTestResult = 
      BuildCoreTestResult BuildCoreResult
    | BuildPerformanceTestResult BuildPerformanceResult
    | BuildOptionalTestResult BuildOptionalResult

data PerformanceType = PerformanceTime | PerformanceSpace

type BatchFile = Text
data BuildTestType = 
      BuildCoreTest (Entity ContestCoreTest) [BuildTest] (Maybe BatchFile)
    | BuildPerformanceTest (Entity ContestPerformanceTest) [BuildTest] (Maybe BatchFile) PerformanceType
    | BuildOptionalTest (Entity ContestOptionalTest) [BuildTest] (Maybe BatchFile)

data BuildTest = BuildTest {
        buildTestInput :: Text
      , buildTestOutput :: Text -- Expected stdout
      , buildTestError :: Text -- Expected stderr
      , buildTestExitCode :: Int -- Expected exit code
    }

instance FromJSON BuildTest where
    parseJSON (Object o) = do
        input <- o .: "input"
        output <- o .:? "output" .!= ""
        error <- o .:? "error" .!= ""
        exitCode <- o .:? "exit" .!= 0
        return $ BuildTest input output error exitCode

    parseJSON _ = mzero

setupFirewall :: MonadIO m => Session -> ErrorT String m ()
setupFirewall session = do
    (Result _ _ exit) <- runSSH "Could not setup firewall" $ execCommand session "sudo iptables -A OUTPUT -m state --state RELATED,ESTABLISHED -j ACCEPT && sudo iptables -A OUTPUT -p tcp --sport 22 -j ACCEPT && sudo iptables -A OUTPUT -j DROP"
    when (exit /= ExitSuccess) $
        fail "Could not setup firewall"

uploadString' :: (MonadIO m) => Session -> FilePath -> ByteString -> String -> ErrorT String m ()
uploadString' session uniqueFilename contents targetFile = do
    let tmpFile = "/tmp/bibifi_" <> uniqueFilename
    liftIO $ BS.writeFile tmpFile contents
    let err = "Could not send file: " <> uniqueFilename
    _ <- runSSH err $ sendFile session 0o666 tmpFile targetFile
    removeIfExists tmpFile
    return ()

uploadString :: (MonadIO m) => Session -> FilePath -> Text -> String -> ErrorT String m ()
uploadString session uniqueFilename contents targetFile = 
    uploadString' session uniqueFilename (Text.encodeUtf8 contents) targetFile


testCompare' :: ByteString -> ByteString -> Bool
testCompare' a b = testCompare (Text.decodeUtf8With lenientDecode a) (Text.decodeUtf8With lenientDecode b)

testCompare :: Text -> Text -> Bool
testCompare t1' t2' = 
    -- Strip whitespace.
    let strip = Text.filter (not . Char.isSpace) in
    let t1 = strip t1' in
    let t2 = strip t2' in
    t1 == t2

data BreakTestCommand = BreakTestCommand {
        breakTestCommandProgram :: String
      , breakTestCommandArgs :: [String]
      , breakTestCommandOutput :: Maybe Text
--       , breakTestCommandError :: String
--       , breakTestCommandExit :: Int
    }

data BreakTest = BreakTestCorrectness {
--        breakTestCorrectnessTarget :: TeamId
        breakTestCorrectnessCommands :: [BreakTestCommand]
      , breakTestCorrectnessBatch :: Maybe Text
    } | BreakTestCrash {
--        breakTestCrashTarget :: TeamId
        breakTestCrashCommands :: [BreakTestCommand]
      , breakTestCorrectnessBatch :: Maybe Text
    } | BreakTestConfidentiality {
        breakTestConfidentialityCommands :: [BreakTestCommand]
      , breakTestConfidentialityLogfile :: String
    } | BreakTestIntegrity {
        breakTestIntegrityCommands :: [BreakTestCommand]
      , breakTestIntegrityLogfile :: String
      , breakTestIntegrityReplacement :: BreakTestReplacements
    }

data BreakTestReplacements = BreakTestReplacements [((Maybe Text),Text)]

instance FromJSON BreakTestReplacements where
    parseJSON (String s) = return $ BreakTestReplacements [(Nothing, s)]
    parseJSON (Array a) = fmap BreakTestReplacements $ 
        Vector.foldM (\acc e -> case e of
            (Object o) -> do
                name <- o .: "name"
                content <- o .: "content"
                return $ (Just name, content):acc
            _ ->
                mzero
        ) [] a
    parseJSON _ = mzero

instance FromJSON BreakTestCommand where
    parseJSON (Object o) = do
        program <- o .: "program"
        if program /= "logappend" && program /= "logread" then
            fail "Program must be logappend or logread"
        else do
            args <- o .: "args"
            output <- o .:? "output"-- .!= ""
--             err <- o .:? "error" .!= ""
--             exitCode <- o .:? "exit" .!= 0
            return $ BreakTestCommand program args output -- err exitCode
    parseJSON _ = mzero

instance FromJSON BreakTest where
    parseJSON (Object o) = do
        (_target :: Int) <- o .: "target_team"
        (typ :: String) <- o .: "type"
        commands <- o .: "commands"
        case typ of 
            "correctness" -> do
                batchM <- o .:? "batch"
                return $ BreakTestCorrectness commands batchM
            "crash" -> do
                batchM <- o .:? "batch"
                return $ BreakTestCorrectness commands batchM
                -- XXX: Temporary to run lowered crashes.
                -- return $ BreakTestCrash commands batchM
                -- XXX
            "integrity" -> do
                log <- o .: "logfile"
                replace <- o .: "replacement"
                return $ BreakTestIntegrity commands log replace
            "confidentiality" -> do
                log <- o .: "logfile"
                return $ BreakTestConfidentiality commands log
            _ ->
                mzero

    parseJSON _ = mzero

data BreakError = 
      BreakErrorSystem String
    | BreakErrorReject String
--    | BreakErrorIncorrect String

instance Error BreakError where
    strMsg = BreakErrorSystem . ("noMsg:default_error:" <>)

data FixError = 
      FixErrorSystem String
    | FixErrorReject String
    | FixErrorBuildFail -- String

instance Error FixError where
    strMsg = FixErrorSystem . ("noMsg:default_error:" <>)

parseCoreTest :: Monad m => Entity ContestCoreTest -> ErrorT String m BuildTestType
parseCoreTest test = parseTest contestCoreTestTestScript test $ \(tests,batchM,_) ->
    return $ Right $ BuildCoreTest test tests batchM

parseOptionalTest :: Monad m => Entity ContestOptionalTest -> ErrorT String m BuildTestType
parseOptionalTest test = parseTest contestOptionalTestTestScript test $ \(tests,batchM,_) ->
    return $ Right $ BuildOptionalTest test tests batchM

parsePerformanceTest :: Monad m => Entity ContestPerformanceTest -> ErrorT String m BuildTestType
parsePerformanceTest test = parseTest contestPerformanceTestTestScript test $ \(tests,batchM,performanceM) ->
    case performanceM of
        Nothing ->
            return $ Left "Performance tests must have a performance type."
        Just performanceType -> 
            return $ Right $ BuildPerformanceTest test tests batchM performanceType

parseTest :: (Monad m, PersistEntity e, Show e) => (e -> Text) -> Entity e -> (([BuildTest],Maybe BatchFile, Maybe PerformanceType) -> m (Either String c)) -> ErrorT String m c
parseTest getTest test f = do
    let parser (Object o) = do
            tests <- o .: "tests"
            batchM <- o .:? "batch"
            (performanceM' :: Maybe Text) <- o .:? "performance"
            performanceM <- case performanceM' of
                  Nothing -> 
                    return Nothing
                  Just "time" ->
                    return $ Just PerformanceTime
                  Just "space" ->
                    return $ Just PerformanceSpace
                  _ ->
                    mzero
            return (tests, batchM, performanceM)
        parser _ = mzero
    json <- ErrorT $ case Aeson.decodeStrict' $ Text.encodeUtf8 $ getTest $ entityVal test of
        Nothing ->
            return $ Left $ "Could not parse test: " <> show test
        Just json ->
            return $ Right json
    ErrorT $ case Aeson.parseEither parser json of
        Left _ -> 
            return $ Left $ "Could not parse test: " <> show test
        Right res ->
            f res
    
createNewUser' :: (MonadIO m, PersistEntity record) => Session -> Entity record -> [Char] -> ErrorT String m (String, Text)
createNewUser' session testE testType = do
    let testIdS = (show $ keyToInt $ entityKey testE)
    let user = "user" <> testIdS <> testType
    let userT = Text.pack user
    createNewUser session user
    return (user, userT)

testLine :: Text -> Text -> ([Text],Int) -> BuildTest -> ([Text],Int)
testLine path user (acc, count') (BuildTest input _output _error _exitCode) = 
    let count = Text.pack $ show count' in
    -- TODO: need to escape input??
    let line = "T=\"$(date +%s%N)\"\n\
            \sudo -i -u " <> user <> " bash -c -i \"/home/builder/test/" <> path <> input <> " > /home/" <> user <> "/" <> count <> "_out 2> /home/" <> user <> "/" <> count <> "_err\" \n\
            \echo $? > /home/" <> user <> "/" <> count <> "_return \n\
            \T=\"$(($(date +%s%N) - T))\"\n\
            \M=\"$((T/1000000))\"\n\
            \echo \"${M}\" > /home/" <> user <> "/" <> count <> "_time \n"
    in
    (line:acc, count' + 1)

generateTestScript' :: Text -> Text -> [BuildTest] -> Text
generateTestScript' path userT testIOs = 
    let script = mconcat $ List.intersperse "\n" $ "#!/bin/bash":(List.reverse $ fst $ List.foldl' (testLine path userT) ([],1) testIOs) in
    Text.decodeUtf8 $ B64.encode $ Text.encodeUtf8 script

uploadTestScript :: (MonadIO m, PersistEntity record) => Session -> Key record -> Text -> ErrorT String m ()
uploadTestScript session submissionId scriptB64 = do
    uploadString session ("test_script" <> show (keyToInt submissionId)) scriptB64 "/home/ubuntu/scriptB64"
    let err = "Could not decode test script"
    (Result _ _ exit) <- runSSH err $ execCommand session "base64 -d /home/ubuntu/scriptB64 > /home/ubuntu/script.sh"
    when (exit /= ExitSuccess) $ 
        fail err

-- For build-it tests.
maybeUploadBatchScript :: (MonadIO m, PersistEntity record) => Session -> String -> Key record -> Maybe Text -> ErrorT String m ()
maybeUploadBatchScript session user submissionId batchM =
    whenJust batchM $ \batchB64 -> do
        -- Should already be base64 encoded. 
        uploadString session ("batch64_" <> show (keyToInt submissionId)) batchB64 "/home/ubuntu/batch64"
        -- putLog $ "sudo -i -u " <> user <> " bash -c \"base64 -d /home/ubuntu/batch64 > /home/" <> user <> "/batch\""
        (Result _ _ exit) <- runSSH "Could not decode batch file" $ execCommand session $ "sudo -i -u " <> user <> " bash -c \"base64 -d /home/ubuntu/batch64 > /home/" <> user <> "/batch\""
        when (exit /= ExitSuccess) $ do
            fail "Could not decode batch file"

-- DO NOT use for break-it. Could leak information. 
parseTestResults :: (IsString e, Functor m, MonadIO m, Error e) => String -> Session -> [BuildTest] -> ErrorT e m (Either String Double)
parseTestResults user session testIOs = 
    fmap fst $ foldM (parseTestResult user session) (Right 0, 1) testIOs

    where
        parseTestResult _ _ (Left err, count) _ = return (Left err, count + 1)
        parseTestResult user session (Right time, count') (BuildTest _input expOutput _expError expExitCode) = do
            let count = show count'
        
            -- Get std output.
            (Result out' _ exit) <- runSSH "Could not get std out" $ execCommand session $ "sudo cat /home/" <> user <> "/" <> count <> "_out"
            when (exit /= ExitSuccess) $
                fail "Could not get std out"
            let out = Text.decodeUtf8With lenientDecode out'
        
            -- Get std error.
            (Result err' _ exit) <- runSSH "Could not get std err" $ execCommand session $ "sudo cat /home/" <> user <> "/" <> count <> "_err"
            when (exit /= ExitSuccess) $
                fail "Could not get std err"
            let _err = Text.decodeUtf8With lenientDecode err'
        
            -- Get exit code.
            (Result exitCode' _ exit) <- runSSH "Could not get exit code" $ execCommand session $ "sudo cat /home/" <> user <> "/" <> count <> "_return"
            when (exit /= ExitSuccess) $
                fail "Could not get exit code"
            exitCode <- ErrorT $ case Text.readMaybe $ BS8.unpack exitCode' of
                Nothing ->
                    return $ Left "Could not parse exit code"
                Just e ->
                    return $ Right e
        
            -- Get time.
            (Result time' _ exit) <- runSSH "Could not get time" $ execCommand session $ "sudo cat /home/" <> user <> "/" <> count <> "_time"
            when (exit /= ExitSuccess) $
                fail "Could not get time"
            time' <- ErrorT $ case Text.readMaybe $ BS8.unpack time' of
                Nothing ->
                    return $ Left "Could not parse time"
                Just t ->
                    return $ Right t
        
            -- Compare result to expected result. 
            if testCompare out expOutput && expExitCode == exitCode then -- testCompare err expError && 
                return (Right (time + time'), count'+1)
            else do
                -- putLog "Failed:"
                -- putLog $ show out
                -- putLog $ show expOutput
                -- putLog $ show err
                -- putLog $ show expError
                -- putLog $ show expExitCode
                -- putLog $ show exitCode
                let err = "Expected(" <> show expExitCode <> "): '" <> show expOutput <> "'. Got(" <> show exitCode <> "): '" <> show out <> "'."
                return (Left err, count'+1)

maybeUploadBatch :: (MonadIO m, PersistEntity record) => Session -> Key record -> Maybe Text -> String -> String -> ErrorT String m ()
maybeUploadBatch _ _ Nothing _ _ = 
    return ()
maybeUploadBatch session submissionId (Just batch64) destFile user = do
    let batch = B64.decodeLenient $ Text.encodeUtf8 batch64
    let uniq = "_batch_" <> show (keyToInt submissionId)
    let tmp = "/tmp/_bibifi_maybeUploadBatch"
    uploadString' session uniq batch tmp
    sudoMove session tmp destFile user

sudoMove :: (IsString e, MonadIO m, Error e) => Session -> String -> String -> String -> ErrorT e m ()
sudoMove session src tgt user = do
    putLog "Moving files."
    (Result _ _ exit) <- runSSH "Could not move file" $ execCommand session $ "sudo mv " <> src <> " " <> tgt
    when (exit /= ExitSuccess) $ 
        fail "Could not move file"

    (Result _ _ exit) <- runSSH "Could not chown file" $ execCommand session $ "sudo chown -R " <> user <> " " <> tgt
    when (exit /= ExitSuccess) $ 
        fail "Could not chown file"

-- | Provided for backwards compatibility. Shouldn't use this anymore.
runnerEC2Configuration :: RunnerOptions -> EC2Configuration
runnerEC2Configuration = f . runnerCloudConfiguration
    where
        f (CloudEC2Configuration c) = c
        f _ = error "runnerEC2Configuration: Not given a CloudConfigurationEC2"

