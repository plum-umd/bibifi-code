module Core.Modular.Shared where

import Control.Monad.Error
import Core (keyToInt)
import Data.Aeson (FromJSON(..))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
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

