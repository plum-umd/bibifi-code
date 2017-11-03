module Config (ConfigKey(..), getConfig, setConfig) where

import Prelude
import LYesod
import Data.Text (Text)
import Database.LPersist
-- import Foundation
import LMonad
import LMonad.Label.DisjunctionCategory
import Model

-- TODO: Maybe move this typeclass somewhere else if it is generalizable?
class ToText a where
    toText :: a -> Text

data ConfigKey =
    DefaultContest

instance ToText ConfigKey where
    toText DefaultContest = "default_contest"

-- TODO: memoize to reduce db queries?

getConfig :: (YesodPersistBackend app ~ SqlBackend, YesodLPersist app, LMonad (HandlerT app IO)) => ConfigKey -> LMonadT (DCLabel Principal) (HandlerT app IO) (Maybe Text)
getConfig key = do
    runDB $ do
        res <- getBy $ UniqueKey $ toText key
        return $ case res of
            Nothing ->
                Nothing
            Just (Entity _ (Configuration _ v)) ->
                Just v

-- TODO: improve this to use one query using something like repsert..
setConfig :: (YesodLPersist app, LMonad (HandlerT app IO), YesodPersistBackend app ~ SqlBackend) => ConfigKey -> Text -> LMonadT (DCLabel Principal) (HandlerT app IO) ()
setConfig key' value = 
    let key = toText key' in
    let new = Configuration key value in
    runDB $ do
        -- Check if it already exists.
        res <- getBy $ UniqueKey key
        case res of 
            Nothing ->
                insert_ new
            Just (Entity id' _) ->
                replace id' new
            
