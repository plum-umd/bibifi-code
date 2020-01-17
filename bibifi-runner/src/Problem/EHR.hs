{-# LANGUAGE OverloadedStrings #-}

module Problem.EHR where

import Core (keyToInt)
import Control.Monad.Error
import qualified Data.Aeson as Aeson
import qualified Data.IORef.Lifted as IO
import qualified Data.List as List
-- import Data.Monoid
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import qualified Database.Esqueleto as E
import Network.SSH.Client.SimpleSSH
import qualified System.FilePath as FilePath
import Yesod.Form.Fields (Textarea(..))

import Cloud
import Common
-- import Core.DatabaseM
-- import Core.Modular.Class
import Core.Score
import Core.SSH
import Problem.Class
import Problem.Shared
import Scorer.Class

newtype EHRSpec = EHRSpec (Entity Contest)

instance ExtractContest EHRSpec where
    extractContest (EHRSpec c) = c

-- instance ScorerClass EHRSpec where
--     scoreContestBuild (EHRSpec (Entity cId _)) _ = defaultScoreBuildRound cId
--     scoreContestBreak (EHRSpec (Entity cId _)) _ = defaultScoreBreakRound cId
--     scoreContestFix (EHRSpec (Entity cId _)) _ = defaultScoreFixRound cId

instance ProblemRunnerClass EHRSpec where
    runOracleSubmission (EHRSpec _contest) opts (Entity submissionId submission) =
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
                    -- Send oracle.
                    putLog "Sending oracle files."
                    let oracleFile = runnerProblemDirectory opts ++ "server"
                    let oracleDestFile = "/home/ubuntu/server"
                    _ <- runSSH (OracleErr "Could not send oracle to instance.") $ sendFile session 0o777 oracleFile oracleDestFile

                    -- setupFirewall session

                    runOracle session oracleDestFile inputObject

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

    runBuildSubmission (EHRSpec (Entity contestId _contest)) opts (Entity submissionId submission) = do
        -- Retrieve tests from database.
        coreTests' <- runDB $ selectList [ContestCoreTestContest ==. contestId] []
        performanceTests' <- runDB $ selectList [ContestPerformanceTestContest ==. contestId] []
        optionalTests' <- runDB $ selectList [ContestOptionalTestContest ==. contestId] [Asc ContestOptionalTestName]

        coreDoneRef <- IO.newIORef False
        resultsE <- runErrorT $ do
            -- Parse tests.
            let toPath c f t = (c t, "/home/ubuntu/gradertests/" <> f (entityVal t) <> ".json")
            let coreTests = map (toPath BuildTestCore contestCoreTestName) coreTests'
            let performanceTests = map (toPath BuildTestPerformance contestPerformanceTestName) performanceTests'
            let optionalTests = map (toPath BuildTestOptional contestOptionalTestName) optionalTests'

            -- Make sure build submission tar and MITMs exist.
            archiveLocation <- getBuildArchiveLocation submission opts 

            -- Delete any previous stored results.
            lift $ runDB $ deleteWhere [BuildCoreResultSubmission ==. submissionId]
            lift $ runDB $ deleteWhere [BuildPerformanceResultSubmission ==. submissionId]
            lift $ runDB $ deleteWhere [BuildOptionalResultSubmission ==. submissionId]

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
                requiredResults <- mapM (runTestAt' session) requiredTests
                mapM_ (recordBuildResult submissionId) requiredResults

                -- Indicate core tests passed. 
                coreDoneRef `IO.writeIORef` True

                -- Continue running optional tests.
                mapM_ (\test -> do
                    result <- runTestAt' session test 
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


    runBreakSubmission (EHRSpec (Entity contestId _contest)) opts bsE@(Entity submissionId submission) = undefined {-FIXME-} {-do
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

                -- Upload Oracle.
                putLog "Sending oracle files."
                oracleFileName <- getOracleFileName $ breakSubmissionTargetTeam submission
                let oracleFile = FilePath.joinPath [oracleBasePath, oracleFileName]
                let oracleDestFile = "/tmp/server"
                _ <- runSSH (BreakErrorSystem "Could not send oracle to instance.") $ sendFile session 0o700 oracleFile oracleDestFile
                (Result _ _ exit) <- runSSH (BreakErrorSystem "Could not make oracle runnable") $ execCommand session ("sudo chmod o+x " ++ oracleDestFile)
                when (exit /= ExitSuccess) $ 
                    fail "Could not chmod oracle"


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

                -- Run grader.
                let targetDestFile = "/home/builder/submission/build/server"
                res <- runJSONBreakTest session targetDestFile oracleDestFile breakTest

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
            oracleBasePath = runnerProblemDirectory opts
            breakJSONFile = FilePath.addExtension (FilePath.joinPath [basePath, "repos", submitTeamIdS, "break", breakName, "test"]) "json"
            targetTeamIdS = show $ keyToInt $ breakSubmissionTargetTeam submission
            submitTeamIdS = show $ keyToInt $ breakSubmissionTeam submission
            breakName = Text.unpack $ breakSubmissionName submission

            userFail err = do
                putLog err
                return $ Just (True, False)
            systemFail err = do
                putLog err
                return $ Just (False, False) -}


    runFixSubmission (EHRSpec (Entity contestId _contest)) opts (Entity submissionId submission) = undefined {-FIXME-} {-do
        -- Retrieve tests from database.
        coreTests' <- runDB $ selectList [ContestCoreTestContest ==. contestId] []
        performanceTests' <- runDB $ selectList [ContestPerformanceTestContest ==. contestId, ContestPerformanceTestOptional ==. False] []

        -- Retrieve breaks (that were auto-accepted/not judged) from database.
        breaks'' <- runDB $ E.select $ E.from $ \(fsb `E.InnerJoin` bs) -> do
            E.on (fsb E.^. FixSubmissionBugsBugId E.==. bs E.^. BreakSubmissionId)
            E.where_ (fsb E.^. FixSubmissionBugsFix E.==. E.val submissionId)
            return bs

        resultsE <- runErrorT $ do
            -- Check for description, other constraints.
            checkForFixDescription submission opts

            -- Parse tests.
            let toPath c f t = (c t, "/home/ubuntu/gradertests/" <> f (entityVal t) <> ".json")
            let coreTests = map (toPath BuildTestCore contestCoreTestName) coreTests'
            let performanceTests = map (toPath BuildTestPerformance contestPerformanceTestName) performanceTests'

            -- Verify breaks fixed and filter them (only include automatically tested ones).
            breaks' <- verifyAndFilterBreaksForFix breaks'' $ \bs -> breakSubmissionStatus bs == BreakTested
            
            -- Convert breaks to JSON breaks.
            breaks <- mapM breakTestToJSONBreakTest breaks'

            -- Make sure build submission tar and MITMs exist.
            archiveLocation <- getFixArchiveLocation submission opts 

            let conf = runnerCloudConfiguration opts
            let manager = runnerHttpManager opts
            launchOneInstanceWithTimeout conf manager 60 $ \_inst session -> do
                putLog "Sending build submission."
                let destArchiveLocation = "/home/ubuntu/submission.tar.gz"
                _ <- runSSH (FixErrorSystem "Could not send submission") $ sendFile session 0o666 archiveLocation destArchiveLocation

                -- setupFirewall session

                -- Upload Oracle.
                putLog "Sending oracle files."
                oracleFileName <- getOracleFileName $ fixSubmissionTeam submission
                let oracleFile = FilePath.joinPath [oracleBasePath, oracleFileName]
                let oracleDestFile = "/tmp/server"
                _ <- runSSH (FixErrorSystem "Could not send oracle to instance.") $ sendFile session 0o700 oracleFile oracleDestFile
                (Result _ _ exit) <- runSSH (FixErrorSystem "Could not make oracle runnable") $ execCommand session ("sudo chmod o+x " ++ oracleDestFile)
                when (exit /= ExitSuccess) $ 
                    fail "Could not chmod oracle"

                -- Setup directory.
                (Result _ _ exit) <- runSSH (FixErrorSystem "Could not make test directory.") $ execCommand session "sudo -i -u builder mkdir /home/builder/submission"
                when (exit /= ExitSuccess) $
                    fail "Could not make test directory."

                -- Extract submission.
                putLog "Extracting build submission."
                (Result _ _ exit) <- runSSH (FixErrorSystem "Could not extract submission") $ execCommand session ("cd /home/builder/submission; sudo -u builder tar -xf " <> destArchiveLocation)
                when (exit /= ExitSuccess) $ 
                    fail "Could not extract submission"

                -- Build submission. 
                putLog "Building submission."
                (Result stderr stdout exit) <- runSSH (FixErrorSystem "Could not run make") $ execCommand session "sudo -i -u builder make -B -C /home/builder/submission/fix/code/build"
                when (exit /= ExitSuccess) $
                    throwError $ FixErrorBuildFail stderr stdout

                let (requiredTests, _) = List.partition (isBuildTestRequired . fst) (coreTests <> performanceTests)
                mapM_ (runCoreTest session) requiredTests

                -- Run break tests.
                let targetDestFile = "/home/builder/submission/fix/code/build/server"
                mapM_ (runBreakTest session targetDestFile oracleDestFile) breaks


        -- Record result.
        case resultsE of
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
            updateFix status msg stdout stderr = 
                runDB $ update submissionId [FixSubmissionStatus =. status, FixSubmissionMessage =. msg, FixSubmissionStdout =. stdout, FixSubmissionStderr =. stderr]

            userFail err = do
                putLog err
                return $ Just (True, False)
            systemFail err = do
                putLog err
                return $ Just (False, False)

            oracleBasePath = runnerProblemDirectory opts

            runCoreTest session test = do
                result <- runTestAt' session test
                case result of
                    (_, BuildResult True _ _) ->
                        return ()
                    (test, BuildResult False _ _) ->
                        throwError $ FixErrorRejected $ "Failed core test: " ++ Text.unpack (buildTestName test)

            runBreakTest session targetDestFile oracleDestFile (test, bs) = do
                res <- runJSONBreakTest session targetDestFile oracleDestFile test
                case res of
                    BreakResult (Just False) _ -> 
                        return ()
                    BreakResult (Just True) _ ->
                        throwError $ FixErrorRejected $ "Failed test: " ++ Text.unpack (breakSubmissionName bs)
                    BreakResult Nothing _ ->
                        throwError $ FixErrorRejected $ "Failed test: " ++ Text.unpack (breakSubmissionName bs) -}
        
getOracleFileName targetId = do
    submissionM <- lift $ lift $ runDB $ selectFirst [BuildSubmissionTeam ==. targetId] [Desc BuildSubmissionTimestamp]
    case submissionM of
        Nothing ->
            throwError $ strMsg $ "Could not find build submission for team: " ++ show targetId
        Just (Entity submissionId _) -> lift $ lift $ do
            passedFunc <- helper submissionId ["testfunc1", "testfunc2", "testfunc3"]
            passedFilter <- helper submissionId ["testfilter1", "testfilter2"]
            passedLet <- helper submissionId ["testlet1", "testlet2", "testlet3"]
            if passedLet then
                return ("server" :: String)
            else if passedFilter then
                return "server2"
            else if passedFunc then
                return "server1"
            else
                return "server0"

    where
        helper :: Key BuildSubmission -> [Text.Text] -> DatabaseM Bool
        helper submissionId testNames = do
            r <- runDB $ E.select $ E.from $ \(t `E.InnerJoin` r) -> do
                E.on (r E.^. BuildOptionalResultTest E.==. t E.^. ContestOptionalTestId)
                E.where_ (
                    (r E.^. BuildOptionalResultPass E.==. E.val True)
                    E.&&. (r E.^. BuildOptionalResultSubmission E.==. E.val submissionId)
                    E.&&. ((t E.^. ContestOptionalTestName) `E.in_` (E.valList testNames)))
                return r
            return $ List.length r == List.length testNames

runTestAt' session (test, location) = do
    res <- runTestAt session location
    return ( test, res)

