{-# LANGUAGE ScopedTypeVariables #-}

module Tests (tests) where

import qualified Data.Text as T
import qualified Data.Char as C
import Data.Time (getCurrentTime)
import Database.Esqueleto
import Common

processTest :: ContestId -> [String] -> DatabaseM ()
processTest cId args = case args of
    [] ->
        liftIO $ putStrLn $ show True
    testType':name':input':output':script':args' ->
        let testType = fmap C.toLower testType' in
        let name = T.pack name' in
        let input = T.pack input' in
        let output = T.pack output' in
        let script = T.pack script' in
        do
        case testType of 
            "core" ->
                runDB $ insert_ $ ContestCoreTest cId name input output script
            "performance" ->
                runDB $ insert_ $ ContestPerformanceTest cId name input output script True
            "optional" ->
                runDB $ insert_ $ ContestOptionalTest cId name input output script
            _ ->
                boolFail "error: invalid test type. Must be Core|Performance|Optional."
        processTest cId args'
    _ ->
        boolFail "error: invalid number of arguments while parsing tests"


tests :: Entity Contest -> [String] -> DatabaseM ()
tests (Entity cId c) args = do
    -- Check date
    now <- liftIO getCurrentTime
    if now > (contestBuildStart c) then
        boolFail "error: contest has already started"
    else
        do
        -- This won't work...
        -- -- Delete all previous judgements.
        -- (liftDB g) $ runDB $ delete $ from $ \(r :: SqlExpr (Entity BuildJudgement)) -> return ()
        -- (liftDB g) $ runDB $ delete $ from $ \(r :: SqlExpr (Entity BreakJudgement)) -> return ()
        -- (liftDB g) $ runDB $ delete $ from $ \(r :: SqlExpr (Entity FixJudgement)) -> return ()
        -- -- Delete all previous test results.
        -- (liftDB g) $ runDB $ delete $ from $ \(r :: SqlExpr (Entity BuildCoreResult)) -> return ()
        -- (liftDB g) $ runDB $ delete $ from $ \(r :: SqlExpr (Entity BuildPerformanceResult)) -> return ()
        -- (liftDB g) $ runDB $ delete $ from $ \(r :: SqlExpr (Entity BuildOptionalResult)) -> return ()
        -- -- Delete all previous submissions.
        -- (liftDB g) $ runDB $ delete $ from $ \(r :: SqlExpr (Entity FixSubmissionBugs)) -> return ()
        -- (liftDB g) $ runDB $ delete $ from $ \(r :: SqlExpr (Entity FixSubmission)) -> return ()
        -- (liftDB g) $ runDB $ delete $ from $ \(r :: SqlExpr (Entity BreakSubmission)) -> return ()
        -- (liftDB g) $ runDB $ delete $ from $ \(r :: SqlExpr (Entity BuildSubmission)) -> return ()
        -- Delete all previous tests
        runDB $ delete $ from $ \(r :: SqlExpr (Entity ContestCoreTest)) -> do
            where_ (r ^. ContestCoreTestContest ==. val cId)
            return ()
        runDB $ delete $ from $ \(r :: SqlExpr (Entity ContestPerformanceTest)) -> do
            where_ (r ^. ContestPerformanceTestContest ==. val cId)
            return ()
        runDB $ delete $ from $ \(r :: SqlExpr (Entity ContestOptionalTest)) -> do
            where_ (r ^. ContestOptionalTestContest ==. val cId)
            return ()
        -- Process all the new tests
        processTest cId args
