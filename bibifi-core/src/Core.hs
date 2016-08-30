module Core where

import Data.Int (Int64)
import Data.Text (Text)
import Database.Persist

import Core.Database
import Model

retrieveActiveContestUrl :: GeneralPersistSql site m => m Text
retrieveActiveContestUrl = runDB' $ do
    res <- getBy $ UniqueKey "default_contest"
    case res of
        Nothing ->
            error "There is currently no default contest"
        Just (Entity _ def) -> 
            return $ configurationValue def

retrieveContest :: GeneralPersistSql site m => Text -> m (Entity Contest)
retrieveContest url = runDB' $ do
    c <- getBy $ UniqueContest url
    case c of 
        Nothing -> 
            error "There is currently no default contest"
        Just c ->
            return c
                    
keyToInt :: PersistEntity record => Key record -> Int64
keyToInt k' = case keyToValues k' of 
    [PersistInt64 k] ->
        k
    _ ->
        error "This should never happen with a pgsql db"
