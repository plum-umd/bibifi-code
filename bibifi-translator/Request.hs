module Request where

-- import BuildSubmissions
import Control.Monad
import qualified Data.Char as C
import qualified Data.Text as T
import Database.Persist
import qualified Database.Esqueleto as E
import Common
import PostDependencyType

request :: [String] -> DatabaseM ()
request args' = 
    case args' of 
        [] ->
            usage
        cmd':args ->
            case lookup (fmap C.toLower cmd') dispatch of 
                Nothing ->
                    usage
                Just cmd ->
                    cmd args

dispatch :: [(String, [String] -> DatabaseM ())]  
dispatch = [( "round1", round1), ( "round2", round2), ( "round3", round3)]

usage :: MonadIO m => m ()
usage = maybeFail $ usageDispatch "REQUEST" dispatch

round1 :: [String] -> DatabaseM ()
round1 args = case args of
    teamid':timestamp':commithash':[] ->
        let teamid = toKey teamid' in
        let timestamp = toTimestamp timestamp' in
        let commithash = T.pack commithash' in
        let submission = BuildSubmission teamid timestamp commithash BuildPending Nothing Nothing in
        do
        checkWithinRound timestamp 1
        sId' <- runDB $ insert submission
        liftIO $ putStrLn $ show $ Just $ keyToInt sId'
    _ ->
        maybeFail "error: incorrect number of arguments"

round2 :: [String] -> DatabaseM ()
round2 args = case args of
    submitteamid':targetteamid':timestamp':commithash':name':[] ->
        let submitteamid = toKey submitteamid' in
        let targetteamid = toKey targetteamid' in
        let timestamp = toTimestamp timestamp' in
        let commithash = T.pack commithash' in
        let name = T.pack name' in
        let submission = BreakSubmission submitteamid targetteamid timestamp commithash BreakPending Nothing False Nothing name Nothing Nothing Nothing Nothing Nothing in -- BreakTesting 
        -- let checkSubmissionLimit = do
        --     let limit = 5
        --     previousCount <- runDB $ count [BreakSubmissionTeam ==. submitteamid, BreakSubmissionTargetTeam ==. targetteamid, BreakSubmissionStatus !=. BreakPullFail,BreakSubmissionStatus !=. BreakRejected, BreakSubmissionResult !=. Just BreakIncorrect]
        --     if previousCount >= limit then
        --         -- insert rejected
        --         let submission' = submission {breakSubmissionStatus = BreakRejected, breakSubmissionMessage = Just "Submission limit reached."} in
        --         do
        --         runDB $ insert_ submission'
        --         maybeFail "team has reached limit"
        --     else
        --         return ()
        -- in
        do
        checkWithinRound timestamp 2
        -- checkSubmissionLimit
        -- if submitteamid == targetteamid then
        --     -- insert rejected
        --     let submission' = submission {breakSubmissionStatus = BreakRejected, breakSubmissionMessage = Just "Cannot break yourself."} in
        --     do
        --     runDB $ insert_ submission'
        --     maybeFail "team cannot break itself"
        -- else do
        --     targetTeamM <- runDB $ get targetteamid
        --     Entity contestId _ <- activeContest
        --     case targetTeamM of 
        --         Nothing -> do
        --             let submission' = submission {breakSubmissionStatus = BreakRejected, breakSubmissionMessage = Just "Invalid target team. Doesn't exist."}
        --             runDB $ insert_ submission'
        --             maybeFail "Target team does not exist"
        --         Just tt | teamContestContest tt /= contestId -> do
        --             let submission' = submission {breakSubmissionStatus = BreakRejected, breakSubmissionMessage = Just "Invalid target team. Not in this contest."}
        --             runDB $ insert_ submission'
        --             maybeFail "Target team is not participating in this contest"
        --         Just _ -> do
        --             -- Check that target team made it to break-it.
        --             validBreakTeam <- runDB $ do
        --                 bsId <- selectFirst [BuildSubmissionTeam ==. targetteamid] [Desc BuildSubmissionId]
        --                 case bsId of
        --                     Just (Entity bsId _) -> do
        --                         -- liftIO $ putStrLn $ show bsId
        --                         buildSubmissionPassesRequiredTests contestId bsId
        --                     Nothing ->
        --                         return False
        --                 -- TODO: Debug this (team 202) XXX
        --                 -- bsId <- getLatestBuildSubmissions contestId $ \tc bs -> do
        --                 --     E.where_ (tc E.^. TeamContestId E.==. E.val targetteamid)
        --                 --     E.limit 1
        --                 --     return $ bs E.^. BuildSubmissionId
        --                 -- liftIO $ putStrLn $ show bsId
        --                 -- case bsId of 
        --                 --     [(E.Value bsId)] ->
        --                 --         buildSubmissionPassesRequiredTests contestId bsId
        --                 --     _ ->
        --                 --         return False
        --             if not validBreakTeam then do
        --                 let submission' = submission {breakSubmissionStatus = BreakRejected, breakSubmissionMessage = Just "Invalid target team."}
        --                 runDB $ insert_ submission'
        --                 maybeFail "Target team did not make it to break it."
        --             else do
        sId' <- runDB $ insert submission
        liftIO $ putStrLn $ show $ Just $ keyToInt sId'
    _ ->
        maybeFail "error: incorrect number of arguments"

round3 :: [String] -> DatabaseM ()
round3 args = case args of
    teamid':timestamp':commithash':diffsize':name':bugids' ->
        let teamid = toKey teamid' in
        let timestamp = toTimestamp timestamp' in
        let commithash = T.pack commithash' in
        let diffsize = read diffsize' in
        let name = T.pack name' in
        let bugids = fmap toKey $ removeDuplicates bugids' in
        let submission = FixSubmission teamid timestamp commithash diffsize FixPending Nothing name Nothing Nothing Nothing in -- FixBuilding 
        let isBugValid bugid = do
              res <- runDB $ get bugid
              case res of 
                    Nothing ->
                        return False
                    Just breakSubm ->
                        return $ (breakSubmissionTargetTeam breakSubm) == teamid && ((breakSubmissionResult breakSubm) == Just BreakExploit || (breakSubmissionResult breakSubm) == Just BreakCorrect)
        in
        let getSubmission = runDB $ do
                prevM <- getBy $ UniqueFixSubmissionName teamid name
                case prevM of
                    Nothing ->
                        insert submission
                    Just (Entity sId _) -> do
                        -- Replace old submission's content.
                        replace sId submission
                        -- Delete old fixed bugs.
                        deleteWhere [FixSubmissionBugsFix ==. sId]
                        return sId
        in
        do
        checkWithinRound timestamp 3
        sId' <- getSubmission
        successE <- foldM (\acc bugid -> case acc of
            Left _ -> 
                return acc
            Right () -> do
                -- Check that teamid is bugid's targetteam. otherwise fall through
                valid <- isBugValid bugid
                if (not valid) then
                    return $ Left bugid
                else
                    do
                    runDB $ insert_ $ FixSubmissionBugs sId' bugid
                    return $ Right ()
          ) (Right ()) bugids
        case successE of
            Right () ->
                liftIO $ putStrLn $ show $ Just $ keyToInt sId'
            Left invalidBugId -> do
                do
                -- "rollback" by displaying invalid bug
                runDB $ E.update $ \s -> do
                    let msg = "Invalid break: " ++ show (keyToInt invalidBugId)
                    E.set s [FixSubmissionStatus E.=. E.val FixRejected, FixSubmissionMessage E.=. E.val (Just msg)] -- FixSubmissionStatus E.=. E.val FixInvalidBugId]
                    E.where_ (s E.^. FixSubmissionId E.==. E.val sId')
                -- let submission = submission {fixSubmissionStatus = FixInvalidBugId}
                -- runDB $ replace sId' submission
                maybeFail "invalid bugid"
    _ ->
        maybeFail "error: incorrect number of arguments"

