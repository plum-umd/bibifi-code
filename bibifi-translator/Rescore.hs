module Rescore where

import qualified Data.Char as C
import Database.Persist

import Common
import Score

rescore :: Entity Contest -> [String] -> DatabaseM ()
rescore c args' = case args' of
    [] ->
        usage
    cmd':args ->
        case lookup (fmap C.toLower cmd') dispatch of 
            Nothing ->
                usage
            Just cmd ->
                cmd c args

dispatch :: [(String, Entity Contest -> [String] -> DatabaseM ())]  
dispatch = [ ( "round1", round1), ( "round2", round2),( "round3", round3)]

usage :: MonadIO m => m ()
usage = boolFail $ usageDispatch "RESCORE" dispatch

round1 :: Entity Contest -> [String] -> DatabaseM ()
round1 (Entity contestId _) args' = case args' of
    [] -> do
        rescoreBuildRound contestId
    _ ->
        boolFail "error: incorrect number of argumnets"

round2 :: Entity Contest -> [String] -> DatabaseM ()
round2 (Entity contestId _) args' = case args' of
    [] -> do
        rescoreBreakRound contestId
    _ ->
        boolFail "error: incorrect number of argumnets"

round3 :: Entity Contest -> [String] -> DatabaseM ()
round3 (Entity contestId _) args' = case args' of
    [] -> do
        rescoreFixRound contestId
    _ ->
        boolFail "error: incorrect number of argumnets"

