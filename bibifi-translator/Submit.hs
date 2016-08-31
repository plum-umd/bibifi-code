module Submit where

import Control.Monad
import qualified Data.Char as C
import qualified Database.Esqueleto as E
-- import System.IO

import Common
import PostDependencyType
import Score

submit :: [String] -> DatabaseM ()
submit args' = do
    case args' of 
        [] ->
            usage
        cmd':args ->
            case lookup (fmap C.toLower cmd') dispatch of 
                Nothing ->
                    usage
                Just cmd ->
                    cmd args
    liftIO $ putStrLn $ show True

dispatch :: [(String, [String] -> DatabaseM ())]  
dispatch = [( "round1", round1),( "round2", round2), ( "round3", round3), ( "bayesianscores", bayesianScores)]

usage :: MonadIO m => m ()
usage = boolFail $ usageDispatch "SUBMIT" dispatch

parseMaybe fail args' = case args' of
    result':args ->
        let result = fmap C.toLower result' in
        case result of 
            "nothing" ->
                return ( Nothing, args)
            "just" -> 
                if length args >= 1 then 
                    do
                    -- putStrLn $ head args
                    let time = read $ head args
                    -- putStrLn $ head args
                    return ( Just time, tail args)
                else
                    fail "error: unable to parse maybe. use Nothing|Just _"
            _ ->
                fail "error: unable to parse maybe. use Nothing|Just _"
    _ ->
        fail "error: unable to parse maybe. use Nothing|Just _"

parseBool fail args' = case args' of 
    result':args ->
        let result = fmap C.toLower result' in
        case result of 
            "true" ->
                return ( True, args)
            "false" ->
                return ( False, args)
            _ ->
                fail "error: unable to parse bool. use TRUE|FALSE"
    _ ->
        fail "error: unable to parse bool. use TRUE|FALSE"

round1Built sId args'' = case args'' of
    [] -> 
        -- putStrLn $ show True
        return ()
    testid':cmd':args' ->
        let cmd = fmap C.toLower cmd' in
        case cmd of 
            "core" ->
                let testid = toKey testid' in
                do
                ( pass, args) <- parseBool boolFail args'
                runDB $ E.insert_ $ BuildCoreResult sId testid pass Nothing
                round1Built sId args  
            "performance" ->
                let testid = toKey testid' in
                do
                (result', args) <- parseMaybe boolFail args'
                let result = BuildPerformanceResult sId testid result' Nothing
                -- let result = case result' of 
                --       Nothing ->
                --         BuildPerformanceResult sId testid Nothing
                --       Just time' ->
                --         let time = read time' in
                --         BuildPerformanceResult sId testid $ Just time
                runDB $ E.insert_ result
                round1Built sId args 
            "optional" ->
                let testid = toKey testid' in
                do
                ( pass, args) <- parseBool boolFail args'
                runDB $ E.insert_ $ BuildOptionalResult sId testid pass Nothing
                round1Built sId args  
            _ ->
                boolFail "error: invalid build result. use CORE|PERFORMANCE|OPTIONAL"
    _ ->
        boolFail "error: not enough arguments"

round1 :: [String] -> DatabaseM ()
round1 args' = case args' of
    id':result':args ->
        let result = fmap C.toLower result' in
        let sId = toKey id' in
        let update st = do
              c <- runDB $ E.updateCount $ \s -> do
                E.set s [BuildSubmissionStatus E.=. E.val st]
                E.where_ (s E.^. BuildSubmissionId E.==. E.val sId)
              if c /= 1 then
                boolFail "error: unable to update submission"
              else
                return ()
        in
        case result of
            "built" -> 
                do
                -- putStrLn "here 1"
                -- hFlush stdout
                round1Built sId args
                -- putStrLn "here 2"
                -- hFlush stdout
                update BuildBuilt
                -- putStrLn "here 3"
                -- hFlush stdout
                (E.Entity contestId _) <- activeContest
                rescoreBuildRound contestId
                -- putStrLn "here 4"
                -- hFlush stdout
            "buildfail" ->
                update BuildBuildFail
            "pullfail" ->
                update BuildPullFail
            _ ->
                boolFail "usage: valid arguments are BUILT|BUILDFAIL|PULLFAIL"
    _ ->
        boolFail "error: incorrect number of arguments"

-- TODO: only check eligible teams??
round2 :: [String] -> DatabaseM ()
round2 args' = case args' of 
    id':result':[] ->
        let result = fmap C.toLower result' in
        let sId = toKey id' in
        let update st res = do
            c <- runDB $ E.updateCount $ \s -> do
                E.set s [BreakSubmissionStatus E.=. E.val st, BreakSubmissionResult E.=. E.val res]
                E.where_ (s E.^. BreakSubmissionId E.==. E.val sId)
            if c /= 1 then
                boolFail "error: unable to update submission"
              else
                return ()
        in
        do
        (E.Entity contestId _) <- activeContest
        case result of 
            "correct" ->
                do
                update BreakTested $ Just BreakCorrect
                rescoreBreakRound contestId
            "incorrect" ->
                do
                update BreakTested $ Just BreakIncorrect
                rescoreBreakRound contestId
            "exploit" ->
                do
                update BreakTested $ Just BreakExploit
                rescoreBreakRound contestId
            "pullfail" ->
                update BreakPullFail Nothing
            "rejected" ->
                update BreakRejected Nothing
            "judgementrequired" ->
                update BreakJudging Nothing
            _ ->
                boolFail "usage: valid arguments are CORRECT|INCORRECT|EXPLOIT|PULLFAIL|JUDGEMENTREQUIRED"
    _ ->
        boolFail "error: incorrect number of arguments"
    
round3 :: [String] -> DatabaseM ()
round3 args' = case args' of 
    id':result':[] ->
        let result = fmap C.toLower result' in
        let sId = toKey id' in
        let update st res = do
            c <- runDB $ E.updateCount $ \s -> do
                E.set s [FixSubmissionStatus E.=. E.val st, FixSubmissionResult E.=. E.val res]
                E.where_ (s E.^. FixSubmissionId E.==. E.val sId)
            if c /= 1 then 
                boolFail "error: unable to update submission"
              else
                return ()
        in
        do
        (E.Entity contestId _) <- activeContest
        case result of
            "fixed" ->
                do
                update FixBuilt $ Just FixFixed
                rescoreFixRound contestId
            "notfixed" ->
                do
                update FixBuilt $ Just FixNotFixed
                rescoreFixRound contestId
            "disqualified" ->
                do
                -- assume it was built?
                update FixBuilt $ Just FixDisqualified
                rescoreFixRound contestId
            "buildfail" ->
                update FixBuildFail Nothing
            "pullfail" ->
                update FixPullFail Nothing
            "judgementrequired" ->
                update FixJudging Nothing
            _ ->
                boolFail "usage: valid arguments are FIXED|NOTFIXED|DISQUALIFIED|BUILDFAIL|PULLFAIL|JUDGEMENTREQUIRED"
    _ ->
        boolFail "error: incorrect number of arguments"

bayesianScores :: [String] -> DatabaseM ()
bayesianScores args' = case args' of
    [] ->
        return ()
    sId':score':args ->
        let sId = toKey sId' in
        let score = read score' in
        do
        -- Update the bayesian score.
        c <- runDB $ E.updateCount $ \s -> do
            E.set s [BreakSubmissionBayesianScore E.=. E.val (Just score)]
            E.where_ (s E.^. BreakSubmissionId E.==. E.val sId)
        when (c /= 1) $
            boolFail "error: unable to update submission"
        -- Update rest of the bayesian scores.
        bayesianScores args
    _ -> 
        boolFail "error: incorrect number of arguments"
