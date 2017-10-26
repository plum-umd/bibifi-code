{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Problem.ATM where

import Core (keyToInt)
import Core.DatabaseM
import Core.Score
import Control.Monad.Base
import Control.Monad.Error
import Data.Aeson (FromJSON(..),(.:),(.=), (.:?), Value(..))
import qualified Data.Aeson as Aeson
-- import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.IORef.Lifted as IO
import qualified Data.List as List
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import qualified Database.Esqueleto as E
import Database.Persist
import Network.SSH.Client.SimpleSSH
import qualified System.FilePath as FilePath
-- import qualified System.Random as Random
import Yesod.Form.Fields (Textarea(..))

import Cloud
import Common
import Core.SSH
import Problem.Class
import Problem.Shared hiding (parseCoreTest, parseOptionalTest, parsePerformanceTest)
import Scorer.Class

newtype ATMSpec = ATMSpec (Entity Contest)

instance ExtractContest ATMSpec where
    extractContest (ATMSpec c) = c

instance ScorerClass ATMSpec where
    scoreContestBuild (ATMSpec (Entity cId _)) _ = defaultScoreBuildRound cId
    scoreContestBreak (ATMSpec (Entity cId _)) _ = defaultScoreBreakRound cId
    scoreContestFix (ATMSpec (Entity cId _)) _ = defaultScoreFixRound cId

instance ProblemRunnerClass ATMSpec where
    runOracleSubmission (ATMSpec _contest) opts (Entity submissionId submission) = 
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
                    -- Send oracle bank. 
                    putLog "Sending oracle files."
-- <<<<<<< HEAD:bibifi-runner/src/Core/Modular/ATM.hs
                    let oracleBankFile = runnerOracleDirectory opts ++ "/bank"
                    _ <- runSSH (OracleErr "Could not send bank oracle to instance.") $ sendFile session 0o700 oracleBankFile oracleBankDestFile

                    -- Send oracle atm.
                    let oracleAtmFile = runnerOracleDirectory opts ++ "/atm"
-- =======
--                     let oracleBankFile = runnerProblemDirectory opts ++ "/dist/build/bank/bank"
--                     _ <- runSSH (OracleErr "Could not send bank oracle to instance.") $ sendFile session 0o700 oracleBankFile oracleBankDestFile
-- 
--                     -- Send oracle atm.
--                     let oracleAtmFile = runnerProblemDirectory opts ++ "/dist/build/atm/atm"
-- >>>>>>> da8e7b6e184073bad29969392b915bb67a9eadb5:bibifi-runner/src/Problem/ATM.hs
                    _ <- runSSH (OracleErr "Could not send atm oracle to instance.") $ sendFile session 0o700 oracleAtmFile oracleAtmDestFile

                    -- setupFirewall session

                    -- Run with input. 
                    putLog "Running oracle."
                    let inputObject' = Aeson.object [
                            "inputs" .= (inputObject :: Aeson.Value)
                          , "type" .= ("oracle" :: String)
                          , "oracle_atm" .= oracleAtmDestFile
                          , "oracle_bank" .= oracleBankDestFile
                          , "oracle_bank_settings" .= Aeson.object [
                              "ip" .= ("127.0.0.1" :: String)
                            , "port" .= (3000 :: Int)
                            ]
                          ]
                    let input = B64.encode $ BSL.toStrict $ Aeson.encode inputObject'
                    -- putLog $ show input
                    -- (Result resOut _ _) <- runSSH "Could not run oracle on instance." $ execCommand session $ "sudo /usr/bin/grader " ++ (BS8.unpack input)
                    (Result resOut _ _) <- executioner session "ubuntu" "/usr/bin/grader " input
                    return resOut

                -- Return result.
                case resultE of
                    Left (OracleErr err) -> do
                        putLog err
                        return (Just False)
                    Left OracleErrTimeout -> do
                        return Nothing
                    Right resOut' -> do
                        putLog "Output received."
                        -- Drop newline. 
                        let (resOut, _) = BS.breakSubstring "\n" resOut'
                                
                        -- Store outputs.
                        putLog "Recording oracle result."
                        let (status, output) = case Aeson.decodeStrict' resOut of
                              Nothing -> 
                                (OracleError, Nothing)
                              Just (OracleOutputError err) ->
                                (OracleError, Just err)
                              Just (OracleOutputSuccess out) ->
                                (OracleFinished, Just out)
                        runDB $ update submissionId [OracleSubmissionOutput =. output, OracleSubmissionStatus =. status]
                        return $ Just True

    runBuildSubmission (ATMSpec (Entity contestId _contest)) opts (Entity submissionId submission) = do
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

            -- Make sure build submission tar and MITMs exist.
            archiveLocation <- getBuildArchiveLocation submission opts 

            -- Delete any previous stored results.
            lift $ runDB $ deleteWhere [BuildCoreResultSubmission ==. submissionId]
            lift $ runDB $ deleteWhere [BuildPerformanceResultSubmission ==. submissionId]
            lift $ runDB $ deleteWhere [BuildOptionalResultSubmission ==. submissionId]


            -- Connect to EC2.
            let conf = runnerCloudConfiguration opts
            let manager = runnerHttpManager opts
            launchOneInstanceWithTimeout conf manager 60 $ \_inst session -> do
                putLog "Sending build submission."
                let destArchiveLocation = "/home/ubuntu/submission.tar.gz"
                _ <- runSSH (BuildError "Could not send submission") $ sendFile session 0o666 archiveLocation destArchiveLocation

                -- setupFirewall session

                -- Send MITMs. 
                -- Note: Should be on VM.
                
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
                let (requiredTests, optionalTests) = List.partition (isTestRequired . fst) (coreTests <> performanceTests <> optionalTests')
                port <- IO.newIORef 3000 -- Fix when we can't reuse ports.
                mapM_ (\t -> runATMTest port session builderBaseDir t >>= (lift . lift . runDB . recorder)) requiredTests
                -- lift $ lift $ runDB $ mapM_ recorder requiredResults

                -- Indicate core tests passed. 
                coreDoneRef `IO.writeIORef` True

                -- Continue running optional tests.
                mapM_ (\test -> do
                    result <- runATMTest port session builderBaseDir test
                    lift $ lift $ runDB $ recorder result
                  ) optionalTests

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
            Left BuildErrorTimeout ->
                -- Timeout.
                handleTimeout coreDoneRef
            Right () ->
                buildBuilt
        
        where
            builderBaseDir = "/home/builder/submission/build"
            handleTimeout coreDoneRef = do
                -- Check if core completed.
                coreDone <- IO.readIORef coreDoneRef
                if coreDone then 
                    buildBuilt
                else
                    return Nothing

            recorder (ATMCoreTest testId, ATMBuildTestOutput result msgM _) = do
                insert_ $ BuildCoreResult submissionId testId result msgM
                return $ Right ()
            recorder (ATMPerformanceTest _ testId, ATMBuildTestOutput False msgM _) = do
                insert_ $ BuildPerformanceResult submissionId testId Nothing msgM
                return $ Right ()
            recorder (ATMPerformanceTest _ _, ATMBuildTestOutput _ _ Nothing) = 
                return $ Left "Output did not include time"
            recorder (ATMPerformanceTest _ testId, ATMBuildTestOutput True msgM (Just time)) = do
                insert_ $ BuildPerformanceResult submissionId testId (Just time) msgM
                return $ Right ()
            recorder (ATMOptionalTest testId, ATMBuildTestOutput result msgM _) = do
                insert_ $ BuildOptionalResult submissionId testId result msgM
                return $ Right ()

            buildBuilt = do
                runDB $ update submissionId [BuildSubmissionStatus =. BuildBuilt]
                return $ Just (True, True)

    runBreakSubmission (ATMSpec (Entity contestId _contest)) opts bsE@(Entity submissionId submission) = do
        resultsE <- runErrorT $ do
            checkSubmissionRound2 contestId bsE
            
            (breakTest :: ATMBreakTest) <- loadBreakSubmissionJSON submissionId breakJSONFile

            -- Check integrity and confidentiality limits. 
            case breakTest of
                ATMBreakIntegrityTest _ -> 
                    checkIntegrityLimit bsE
                ATMBreakConfidentialityTest _ ->
                    checkConfidentialityLimit bsE
                ATMBreakCorrectnessTest _ ->
                    return ()
            
            -- Make sure build submission tar exists.
            breakArchiveLocation <- getBreakArchiveLocation submission opts 

            checkForBreakDescription submission opts

            -- Start EC2.
            let conf = runnerCloudConfiguration opts
            let manager = runnerHttpManager opts
            launchOneInstanceWithTimeout conf manager 60 $ \_inst session -> do
                -- Setup firewall.
                -- setupFirewall session

                -- Setup directory.
                -- (Result _ _ exit) <- runSSH (BreakErrorSystem "Could not make test directory.") $ execCommand session "sudo -i -u builder mkdir /home/builder/submission"
                -- when (exit /= ExitSuccess) $
                --     fail "Could not make test directory."

                -- Upload Oracle.
                putLog "Sending oracle files."
                let oracleBankFile = oracleBasePath ++ "/bank"
                _ <- runSSH (BreakErrorSystem "Could not send bank oracle to instance.") $ sendFile session 0o700 oracleBankFile oracleBankDestFile

                let oracleAtmFile = oracleBasePath ++ "/atm"
                _ <- runSSH (BreakErrorSystem "Could not send atm oracle to instance.") $ sendFile session 0o700 oracleAtmFile oracleAtmDestFile

                -- Upload target.
                putLog "Sending target submission."
                let targetArchiveLocation = FilePath.addExtension (FilePath.joinPath [basePath,"round2",targetTeamIdS]) "zip"
                _ <- runSSH (BreakErrorSystem "Could not send target submission") $ sendFile session 0o666 targetArchiveLocation destTargetArchiveLocation

                -- Extract target.
                putLog "Extracting target submission."
                -- putLog $ ("cd /home/builder; sudo -u builder unzip " <> destTargetArchiveLocation <> "; mv ./repos/" <> targetTeamIdS <> "/ ./submission")
                (Result _ _ exit) <- runSSH (BreakErrorSystem "Could not extract submission") $ execCommand session ("cd /home/builder; sudo -u builder unzip " <> destTargetArchiveLocation <> "; sudo -u builder mv ./repos/" <> targetTeamIdS <> " ./submission")
                when (exit /= ExitSuccess) $ 
                    fail "Could not extract target submission"

                -- Build target.
                putLog "Building target submission."
                (Result _ _ exit) <- runSSH (BreakErrorSystem "Building target failed") $ execCommand session $ "sudo -i -u builder make -B -C /home/builder/submission/build"
                when (exit /= ExitSuccess) $
                    fail "Building target failed"
                

                -- Upload mitm.
                putLog "Sending breaker submission."
                _ <- runSSH (BreakErrorSystem "Could not send break submission") $ sendFile session 0o666 breakArchiveLocation destBreakArchiveLocation

                -- Extract mitm.
                putLog "Extracting breaker submission."
                (Result _ _ exit) <- runSSH (BreakErrorSystem "Could not extract break submission") $ execCommand session $ "cd /home/breaker; sudo -i -u breaker unzip " <> destBreakArchiveLocation
                when (exit /= ExitSuccess) $
                    fail "Could not extract break submission"

                -- Build mitm.
                maybeBuildMITM session breakTest

                -- Run grader.
                port <- IO.newIORef 3000 -- Fix when we can't reuse ports.
                input <- fmap (B64.encode . BSL.toStrict . Aeson.encode) $ prepareBreakTest port builderBaseDir destBreakFolder breakTest
                (Result resOut' _ _) <- executioner session "ubuntu" "/usr/bin/grader" input

                -- Drop newline. 
                let (resOut, _) = BS.breakSubstring "\n" resOut'
                putLog $ show resOut

                -- Parse resOut.
                ErrorT $ case Aeson.decodeStrict' resOut of
                    Nothing ->
                        return $ Left $ BreakErrorSystem $ "Could not decode test output: " <> BS8.unpack resOut
                    Just r ->
                        return $ Right (r, breakTest)

        -- record results
        case resultsE of
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
            Right (ATMBuildTestOutput False msgM _, _) -> do
                runDB $ update submissionId [BreakSubmissionStatus =. BreakRejected, BreakSubmissionResult =. Nothing, BreakSubmissionMessage =. fmap Text.unpack msgM, BreakSubmissionStdout =. Nothing, BreakSubmissionStderr =. Nothing]
                userFail $ maybe "Test failed" Text.unpack msgM
            Right (ATMBuildTestOutput True _msgM _, breakTest) -> do
                let result = case breakTest of
                        ATMBreakIntegrityTest _ ->
                            BreakExploit
                        ATMBreakConfidentialityTest _ ->
                            BreakExploit
                        ATMBreakCorrectnessTest _ ->
                            BreakCorrect
                runDB $ update submissionId [BreakSubmissionStatus =. BreakTested, BreakSubmissionResult =. Just result, BreakSubmissionMessage =. Nothing]
                return $ Just (True, True)


        where
            userFail err = do
                putLog err
                return $ Just (True, False)

            systemFail err = do
                putLog err
                return $ Just (False, False)

            builderBaseDir = "/home/builder/submission/build"

            maybeBuildMITM _ (ATMBreakCorrectnessTest _) = return ()
            maybeBuildMITM session (ATMBreakIntegrityTest _) = buildMITM session
            maybeBuildMITM session (ATMBreakConfidentialityTest _) = buildMITM session

            buildMITM session = do
                putLog "Build breaker submission."
                (Result stderr stdout exit) <- runSSH (BreakErrorSystem "Could not run make") $ execCommand session $ "sudo -i -u breaker make -B -C " <> destBreakFolder
                when (exit /= ExitSuccess) $
                    throwError $ BreakErrorBuildFail stderr stdout
                return ()



            destBreakArchiveLocation = "/home/ubuntu/break.tar.gz"
            destTargetArchiveLocation = "/home/ubuntu/submission.zip"
            basePath = runnerRepositoryPath opts
            oracleBasePath = runnerProblemDirectory opts
            breakJSONFile = FilePath.addExtension (FilePath.joinPath [basePath, "repos", submitTeamIdS, "break", breakName, "test"]) "json"
            targetTeamIdS = show $ keyToInt $ breakSubmissionTargetTeam submission
            submitTeamIdS = show $ keyToInt $ breakSubmissionTeam submission
            -- destBreakFolder = "/home/breaker/break/" <> breakName
            destBreakFolder = "/home/breaker"
            breakName = Text.unpack $ breakSubmissionName submission

    runFixSubmission (ATMSpec (Entity contestId _contest)) opts (Entity submissionId submission) = do
        -- Retrieve tests from database.
        coreTests' <- runDB $ selectList [ContestCoreTestContest ==. contestId] []
        performanceTests' <- runDB $ selectList [ContestPerformanceTestContest ==. contestId] []

        -- Retrieve breaks from database.
        breaks' <- runDB $ E.select $ E.from $ \(fsb `E.InnerJoin` bs) -> do
            E.on (fsb E.^. FixSubmissionBugsBugId E.==. bs E.^. BreakSubmissionId)
            E.where_ (fsb E.^. FixSubmissionBugsFix E.==. E.val submissionId)
            return bs
        
        resultE <- runErrorT $ do
            -- Check for description, other constraints.
            checkForFixDescription submission opts

            -- Verify breaks fixed and filter them. 
            breaks <- foldM verifyAndFilterBreaks [] breaks'

            -- Parse tests.
            coreTests <- mapM parseCoreTest coreTests'
            performanceTests <- mapM parsePerformanceTest performanceTests'

            -- Make sure build submission tar and MITMs exist.
            archiveLocation <- getFixArchiveLocation submission opts 

            -- Start EC2.
            let conf = runnerCloudConfiguration opts
            let manager = runnerHttpManager opts
            launchOneInstanceWithTimeout conf manager 60 $ \_inst session -> do
                -- Setup firewall.
                -- setupFirewall session

                -- Upload Oracle.
                putLog "Sending oracle files."
                let oracleBankFile = oracleBasePath ++ "/bank"
                _ <- runSSH (FixErrorSystem "Could not send bank oracle to instance.") $ sendFile session 0o700 oracleBankFile oracleBankDestFile

                let oracleAtmFile = oracleBasePath ++ "/atm"
                _ <- runSSH (FixErrorSystem "Could not send atm oracle to instance.") $ sendFile session 0o700 oracleAtmFile oracleAtmDestFile

                -- Setup directory.
                (Result _ _ exit) <- runSSH (FixErrorSystem "Could not make test directory.") $ execCommand session "sudo -i -u builder mkdir /home/builder/submission"
                when (exit /= ExitSuccess) $
                    fail "Could not make test directory."

                -- Upload fix submission.
                putLog "Sending fix submission."
                let destArchiveLocation = "/home/ubuntu/submission.tar.gz"
                _ <- runSSH (FixErrorSystem "Could not send submission") $ sendFile session 0o666 archiveLocation destArchiveLocation

                -- Extract fix.
                putLog "Extracting fix submission."
                (Result _ _ exit) <- runSSH (FixErrorSystem "Could not extract submission") $ execCommand session ("cd /home/builder/submission; sudo -u builder tar -xf " <> destArchiveLocation)
                when (exit /= ExitSuccess) $ 
                    fail "Could not extract submission"

                -- Build fix.
                putLog "Building fix."
                (Result stderr stdout exit) <- runSSH (FixErrorSystem "Could not run make") $ execCommand session "sudo -i -u builder make -B -C /home/builder/submission/fix/code/build"
                when (exit /= ExitSuccess) $
                    throwError $ FixErrorBuildFail stderr stdout

                -- Run core tests.
                let (requiredTests, _) = List.partition (isTestRequired . fst) (coreTests <> performanceTests)
                port <- IO.newIORef 3000 -- Fix when we can't reuse ports.
                mapM_ (runCoreTest port session) requiredTests

                -- Run correctness break tests.
                mapM_ (runBreakTest port session) breaks

        -- Record result.
        case resultE of
            Left (FixErrorSystem err) -> 
                systemFail err
            Left FixErrorTimeout ->
                return Nothing
            Left (FixErrorBuildFail stdout' stderr') -> do
                let stdout = Just $ Textarea $ Text.decodeUtf8With Text.lenientDecode stdout'
                let stderr = Just $ Textarea $ Text.decodeUtf8With Text.lenientDecode stderr'
                updateFix FixRejected (Just "Running make failed") stdout stderr
                userFail "Build failed"
            Left (FixErrorRejected msg) -> do
                updateFix FixRejected (Just msg) Nothing Nothing
                userFail msg
            Right () -> do
                updateFix FixJudging Nothing Nothing Nothing
                return $ Just (True, True)


        where
            userFail err = do
                putLog err
                return $ Just (True, False)
            systemFail err = do
                putLog err
                return $ Just (False, False)

            updateFix status msg stdout stderr = 
                runDB $ update submissionId [FixSubmissionStatus =. status, FixSubmissionMessage =. msg, FixSubmissionStdout =. stdout, FixSubmissionStderr =. stderr]

            runBreakTest port session (Entity bsId breakTest') = do
                breakTest <- case breakSubmissionJson breakTest' of
                    Nothing ->
                        throwError $ FixErrorSystem $ "Break json not stored: " ++ show (keyToInt bsId)
                    Just json -> case Aeson.eitherDecode' $ BSL.fromStrict $ Text.encodeUtf8 json of
                        Left e -> do
                            throwError $ FixErrorSystem $ "Could not parse test: " ++ e
                        Right r -> 
                            return r

                -- Run grader.
                input <- fmap (B64.encode . BSL.toStrict . Aeson.encode) $ prepareBreakTest port builderBaseDir "UNUSED" breakTest
                (Result resOut' _ _) <- executioner session "ubuntu" "/usr/bin/grader" input

                -- Drop newline.
                let (resOut, _) = BS.breakSubstring "\n" resOut'
                putLog $ show resOut

                -- Parse resOut.
                case Aeson.decodeStrict' resOut of
                    Nothing ->
                        throwError $ FixErrorSystem $ "Could not decode test output: " <> BS8.unpack resOut
                    Just (ATMBuildTestOutput True _msgM _) -> 
                        throwError $ FixErrorRejected $ "Did not pass break test '" <> Text.unpack (breakSubmissionName breakTest') <> "' (" <> show (keyToInt bsId) <> ")."
                    Just (ATMBuildTestOutput False _msgM _) ->
                        return ()

            oracleBasePath = runnerProblemDirectory opts

            builderBaseDir = "/home/builder/submission/fix/code/build"

            runCoreTest portRef session test = do
                (_, res) <- runATMTest portRef session builderBaseDir test
                case res of 
                    ATMBuildTestOutput True _ _ -> 
                        return ()
                    ATMBuildTestOutput False _ _ -> do
                        throwError $ FixErrorRejected "Failed core tests"

            verifyAndFilterBreaks acc bsE@(Entity bsId bs) = do
                -- Check that this break is only being fixed once.
                fixes <- lift $ runDB $ E.select $ E.from $ \(fs `E.InnerJoin` fsb) -> do
                    E.on (fs E.^. FixSubmissionId E.==. fsb E.^. FixSubmissionBugsFix)
                    E.where_ (fsb E.^. FixSubmissionBugsBugId E.==. E.val bsId E.&&. (fs E.^. FixSubmissionStatus E.!=. E.val FixRejected E.&&. fs E.^. FixSubmissionStatus E.!=. E.val FixBuildFail E.&&. fs E.^. FixSubmissionStatus E.!=. E.val FixInvalidBugId))
                    return fsb -- TODO: E.countRows
                when (List.length fixes > 1) $
                    throwError $ FixErrorRejected $ "Already fixed break '" <> Text.unpack (breakSubmissionName bs) <> "' (" <> show (keyToInt bsId) <> ")"

                -- Include break if it's a correctness violation.
                if breakSubmissionBreakType bs == Just BreakCorrectness then
                    return $ bsE:acc
                else
                    return acc

                
prepareBreakTest :: (MonadIO m, MonadBase IO m) => IO.IORef Int -> String -> String -> ATMBreakTest -> m Value
prepareBreakTest port baseDir destBreakFolder breakTest = do
    -- (bankPort, commandPort, bankOraclePort, mitmPort) <- generateRandomPorts
    bankPort <- getNextPort port
    commandPort <- getNextPort port
    bankOraclePort <- getNextPort port
    mitmPort <- getNextPort port
    let input' = [
            "type" .= breakTestType breakTest
          , "atm" .= (baseDir ++ "/atm" :: String)
          , "bank" .= (baseDir ++ "/bank" :: String)
          , "bank_settings" .= Aeson.object [
                "ip" .= ("127.0.0.1" :: String)
              , "port" .= (bankPort :: Int)
              ]
          , "atm_user" .= ("client" :: String)
          , "bank_user" .= ("server" :: String)
          , "oracle_atm" .= oracleAtmDestFile
          , "oracle_bank" .= oracleBankDestFile
          , "command_settings" .= Aeson.object [
                "ip" .= ("127.0.0.1" :: String)
              , "port" .= (commandPort :: Int)
              ]
          , "oracle_bank_settings" .= Aeson.object [
                "ip" .= ("127.0.0.1" :: String)
              , "port" .= (bankOraclePort :: Int)
              ]
          , "mitm" .= (FilePath.joinPath [destBreakFolder,"mitm"])
          , "mitm_settings" .= Aeson.object [
                "ip" .= ("127.0.0.1" :: String)
              , "port" .= (mitmPort :: Int)
              ]
          , "atm_user" .= ("client" :: String)
          , "bank_user" .= ("server" :: String)
          , "mitm_user" .= ("breaker" :: String)
          ]
    return $ Aeson.object $ includeTests breakTest input'

    where
        -- generateRandomPorts = do
        --     gen'''' <- liftIO Random.getStdGen
        --     let (bankPort, gen''') = generateRandomPort gen'''' []
        --     let (commandPort, gen'') = generateRandomPort gen''' [bankPort]
        --     let (bankOraclePort, gen') = generateRandomPort gen'' [bankPort, commandPort]
        --     let (mitmPort, _) = generateRandomPort gen' [bankPort, commandPort, bankOraclePort]
        --     return ( bankPort, commandPort, bankOraclePort, mitmPort)

        --     where
        --         generateRandomPort :: (Random.RandomGen g) => g -> [Int] -> (Int, g)
        --         generateRandomPort gen prev = 
        --             let res@(port, gen') = Random.randomR (1024, 65535) gen in
        --             if List.elem port prev then
        --                 generateRandomPort gen' prev
        --             else
        --                 res

        breakTestType :: ATMBreakTest -> String
        breakTestType (ATMBreakCorrectnessTest _) = "correctness"
        breakTestType (ATMBreakIntegrityTest _) = "integrity"
        breakTestType (ATMBreakConfidentialityTest _) = "confidentiality"

        includeTests (ATMBreakCorrectnessTest t) tt = ("tests" .= t):tt
        includeTests (ATMBreakIntegrityTest t) tt = maybe tt (\a -> ("tests" .= a):tt) t
        includeTests (ATMBreakConfidentialityTest t) tt = maybe tt (\a -> ("tests" .= a):tt) t

data ATMBreakTest = 
      ATMBreakCorrectnessTest Aeson.Value
    | ATMBreakIntegrityTest (Maybe Aeson.Value)
    | ATMBreakConfidentialityTest (Maybe Aeson.Value)

instance FromJSON ATMBreakTest where
    parseJSON (Object o) = do
        (_target :: Int) <- o .: "target_team"
        (typ :: String) <- o .: "type"
        inputsM <- o .:? "inputs"
        case typ of
            "correctness" -> case inputsM of
                Nothing -> 
                    fail "No 'inputs' provided."
                Just inputs -> 
                    return $ ATMBreakCorrectnessTest inputs
            "integrity" ->
                return $ ATMBreakIntegrityTest inputsM
            "confidentiality" ->
                return $ ATMBreakConfidentialityTest inputsM
            _ ->
                mzero
    parseJSON _ = mzero

-- instance ToJSON ATMBreakTest where
--     toJSON (ATMBreakCorrectnessTest _) = undefined
--     toJSON (ATMBreakIntegrityTest _) = undefined
--     toJSON (ATMBreakConfidentialityTest _) = undefined

instance ModularBreakTest ATMBreakTest where
    breakTestToType (ATMBreakCorrectnessTest _) = BreakCorrectness
    breakTestToType (ATMBreakIntegrityTest _) = BreakIntegrity
    breakTestToType (ATMBreakConfidentialityTest _) = BreakConfidentiality

data ATMTest = 
      ATMCoreTest {
          atmCoreTestId :: ContestCoreTestId
        }
    | ATMPerformanceTest {
          atmPerformanceTestRequired :: Bool
        , atmPerformanceTestId :: ContestPerformanceTestId
        }
    | ATMOptionalTest {
          atmOptionalTestId :: ContestOptionalTestId
        }

oracleDestDirectory :: String
oracleDestDirectory = "/tmp/"

oracleAtmDestFile :: String
oracleAtmDestFile = oracleDestDirectory ++ "atm"

oracleBankDestFile :: String
oracleBankDestFile = oracleDestDirectory ++ "bank"

isTestRequired :: ATMTest -> Bool
isTestRequired (ATMCoreTest _) = True
isTestRequired (ATMPerformanceTest required _) = required
isTestRequired (ATMOptionalTest _) = False

runATMTest :: (BackendError e, MonadIO m, MonadBase IO m) => IO.IORef Int -> Session -> String -> (ATMTest, ATMBuildTestInput) -> ErrorT e m (ATMTest, ATMBuildTestOutput)
runATMTest portRef session baseDir (test, (ATMBuildTestInput inputs mitm)) = do
    port <- getNextPort portRef
    mitmPort <- getNextPort portRef
    let input' = [
            "type" .= ("buildit" :: String)
          , "tests" .= inputs
          , "atm" .= (baseDir ++ "/atm" :: String)
          , "bank" .= (baseDir ++ "/bank" :: String)
          , "bank_settings" .= Aeson.object [
                "ip" .= ("127.0.0.1" :: String)
              , "port" .= port
              ]
--           , "oracle_atm" .= oracleAtmDestFile
--           , "oracle_bank" .= oracleBankDestFile
--           , "oracle_bank_settings" .= Aeson.object [
--                 "ip" .= ("127.0.0.1" :: String)
--               , "port" .= (6300 :: Int)
--               ]
          , "atm_user" .= ("client" :: String)
          , "bank_user" .= ("server" :: String)
          ]
    let inputObject = case mitm of
          Nothing -> 
            Aeson.object input'
          Just mitm ->
            let mitmLoc = "mitm" .= (FilePath.joinPath ["/home/ubuntu/mitm",mitm]) in
            let mitmSettings = "mitm_settings" .= Aeson.object [
                    "ip" .= ("127.0.0.1" :: String)
                  , "port" .= mitmPort
                  ]
            in
            Aeson.object $ mitmLoc:mitmSettings:input'
    let input = B64.encode $ BSL.toStrict $ Aeson.encode inputObject
    -- putLog $ show input
    -- (Result resOut' _ _) <- runSSH (BuildError "Could not run build test on instance.") $ execCommand session $ "sudo /usr/bin/grader " <> (BS8.unpack input)
    (Result resOut' _ _) <- executioner session "ubuntu" "/usr/bin/grader" input
    
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

data ATMBuildTestInput = ATMBuildTestInput {
      atmBuildInput :: Aeson.Value
    , atmBuildMITM :: (Maybe String)
    }

data ATMBuildTestOutput = ATMBuildTestOutput {
      atmBuildOutput :: Bool
    , atmBuildMessage :: Maybe Text
    , atmBuildTestTime :: Maybe Double
    }

instance FromJSON ATMBuildTestOutput where
    parseJSON (Object o) = do
        res <- o .: "result"
        message <- o .:? "error"
        time <- o .:? "time"
        return $ ATMBuildTestOutput res message time
    parseJSON _ = mzero

instance FromJSON ATMBuildTestInput where
    parseJSON (Object o) = do
        inputs <- o .: "inputs"
        mitm <- o .:? "mitm"
        return $ ATMBuildTestInput inputs mitm
    parseJSON _ = mzero

parseCoreTest :: (Error e, Monad m) => Entity ContestCoreTest -> ErrorT e m (ATMTest, ATMBuildTestInput)
parseCoreTest = parseTestHelper contestCoreTestTestScript (ATMCoreTest . entityKey)

parsePerformanceTest :: (Error e, Monad m) => Entity ContestPerformanceTest -> ErrorT e m (ATMTest, ATMBuildTestInput)
parsePerformanceTest t@(Entity _ test) = 
    let req = not $ contestPerformanceTestOptional test in
    parseTestHelper contestPerformanceTestTestScript (ATMPerformanceTest req . entityKey) t

parseOptionalTest :: (Monad m, Error e) => Entity ContestOptionalTest -> ErrorT e m (ATMTest, ATMBuildTestInput)
parseOptionalTest = parseTestHelper contestOptionalTestTestScript (ATMOptionalTest . entityKey)

-- Ugly fix, but works.
getNextPort portRef = do
    port <- IO.readIORef portRef
    IO.writeIORef portRef $ port + 1
    return port
