module Rescore where

import qualified Data.Char as C
import Database.Persist

import Common
import Score

rescore :: [String] -> DatabaseM ()
rescore args' = case args' of
    [] ->
        usage
    cmd':args ->
        case lookup (fmap C.toLower cmd') dispatch of 
            Nothing ->
                usage
            Just cmd ->
                cmd args

dispatch :: [(String, [String] -> DatabaseM ())]  
dispatch = [ ( "round1", round1), ( "round2", round2),( "round3", round3)]

usage :: MonadIO m => m ()
usage = boolFail $ usageDispatch "RESCORE" dispatch

round1 :: [String] -> DatabaseM ()
round1 args' = case args' of
    [] -> do
        (Entity contestId _) <- activeContest
        rescoreBuildRound contestId
    _ ->
        boolFail "error: incorrect number of argumnets"

round2 :: [String] -> DatabaseM ()
round2 args' = case args' of
    [] -> do
        (Entity contestId _) <- activeContest
        rescoreBreakRound contestId
    _ ->
        boolFail "error: incorrect number of argumnets"

round3 :: [String] -> DatabaseM ()
round3 args' = case args' of
    [] -> do
        (Entity contestId _) <- activeContest
        rescoreFixRound contestId
    _ ->
        boolFail "error: incorrect number of argumnets"

