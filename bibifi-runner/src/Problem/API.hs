module Problem.API where

import Control.Monad
import Control.Monad.Error
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
-- import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Network.SSH.Client.SimpleSSH
import qualified System.Directory as Directory
import System.FilePath ((</>))

import Cloud
import Common
import Core.Score
import Core.SSH
import Problem.Class
import Problem.Shared
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

                    -- Make oracle directory.
                    (Result _ _ exit) <- runSSH (OracleErr "Could not make oracle directory.") $ 
                        execCommand session "sudo -i mkdir -p /oracle"
                    when (exit /= ExitSuccess) $
                        fail "Could not make oracle directory."

                    (Result _ _ exit) <- runSSH (OracleErr "Could not update oracle directory permissions.") $ 
                        execCommand session "sudo -i chmod -R 0700 /oracle"
                    when (exit /= ExitSuccess) $
                        fail "Could not update oracle directory permissions."

                    -- Send oracle.
                    putLog "Sending oracle files."
                    oracleFiles <- getOracleFiles
                    sendFiles session oracleFiles

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
            getOracleFiles = liftIO $ do
                let dir = runnerOracleDirectory opts
                contents <- listDirectory dir
                let files = fmap (\f -> (dir </> f, "/oracle" </> f)) contents
                filterM (Directory.doesFileExist . fst) files


            sendFiles session = mapM_ $ \(localF, remoteF) -> 
                runSSH (OracleErr $ "Could not send file: " ++ localF) $ sendFile session 0o777 localF remoteF

            runOracle :: (BackendError e, MonadIO m) => Session -> Aeson.Value -> ErrorT e m (Maybe OracleOutput)
            runOracle session input = do
                let json = BSL.toStrict $ Aeson.encode $ Aeson.object [
                        "type" .= ("oracle" :: String)
                      , "input" .= input
                      ]

                -- Upload json input.
                uploadString session json destJson

                -- Launch grader.
                (Result resOut' _ _) <- executioner' session testUser grader [destJson]

                -- Drop newline.
                let (resOut, _) = BS.breakSubstring "\n" resOut'
                -- putLog $ show resOut

                -- Parse resOut.
                return $ Aeson.decodeStrict' resOut

            listDirectory path = filter f <$> Directory.getDirectoryContents path
                where f filename = filename /= "." && filename /= ".."

    runBuildSubmission = undefined
    runBreakSubmission = undefined
    runFixSubmission = undefined
