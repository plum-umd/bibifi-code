{-# LANGUAGE RankNTypes,FlexibleInstances #-}

module Common (
      module Control.Monad.IO.Class
    , module Core.DatabaseM
    , module Model
    , activeContest
    , silentFail
    , maybeFail
    , boolFail
    , usageDispatch
    , keyToInt
    , toKey
    , checkWithinRound
    , removeDuplicates
    , toTimestamp
    ) where

import Prelude
import System.IO
import System.Exit
import Data.ByteString (ByteString)
import Database.Persist
import Database.Persist.Postgresql (PostgresConf)
import Yesod.Default.Config
import Data.Yaml (decodeFile, parseMonad)
import qualified Data.Map as M
import qualified Data.Time.Clock.POSIX as P
import Data.Text
import Data.Monoid.Action
import Core
import Core.DatabaseM
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Resource
import Control.Monad.Logger
import Model
import Yesod.Core
import Yesod ( share, mkPersist, sqlOnlySettings, mkMigrate, persistFileWith)
import Database.Persist.Quasi
import Data.Time
import PostDependencyType
import Data.Typeable (Typeable)
import Data.List as L
import qualified Data.Char as C
import qualified Database.Esqueleto as E
import Data.Hashable

-- Setup DB stuff.

-- type PersistConf = PostgresConf
-- 
-- share [mkPersist sqlOnlySettings, mkMigrate "migrateAll"]
--     $(persistFileWith lowerCaseSettings "config/models")
-- 
-- data WorkerConf = WorkerConf { 
--         getPool :: Database.Persist.PersistConfigPool PersistConf,
--         getConfig :: PersistConf
--     }
-- 
-- type WorkerM = ReaderT WorkerConf (ResourceT (LoggingT IO))
-- 
-- data Global = Global {
--         liftDB :: forall a. WorkerM a -> IO a
--     }
-- 
-- runDB f = do
--   workerConf <- ask
--   Database.Persist.runPool (getConfig workerConf) f (getPool workerConf)
-- 
-- -- dbRunner = defaultGetDBRunner getPool
-- 
-- -- https://github.com/yesodweb/yesod/wiki/Using-Database.Persist.runPool-without-Foundation
-- justEnv :: Text -> M.Map Text Value -> Value
-- justEnv envName obj = 
--   case M.lookup envName obj of
--       Nothing -> error "could not find environment"
--       Just env -> env
-- 
-- loadYaml :: String -> IO (M.Map Text Value)
-- loadYaml fp = do
--   mval <- decodeFile fp
--   case mval of
--       Nothing -> error $ "Invalid YAML file: " ++ show fp
--       Just o -> return o
-- 
-- makeGlobal :: IO Global
-- makeGlobal = 
--     do
--     dbConfJson <- justEnv "Translator" `fmap` loadYaml "config/postgresql.yml"
--     dbConf <- parseMonad Database.Persist.loadConfig dbConfJson
--     p <- Database.Persist.createPoolConfig (dbConf :: PersistConf)
--     let workerConf = WorkerConf p dbConf
--     return $ Global $ \f -> runStderrLoggingT $ runResourceT $ flip runReaderT workerConf f -- runStdoutLoggingT 

-- Common functions.

activeContest :: DatabaseM (Entity Contest)
activeContest = do
    res <- runDB $ getBy $ UniqueKey "default_contest"
    case res of
        Nothing ->
            error "there is currently no default contest"
        Just (Entity _ def) ->
            do
            c <- runDB $ getBy $ UniqueContest $ configurationValue def
            case c of 
                Nothing -> 
                    error "there is currently no default contest"
                Just c ->
                    return $ c

removeDuplicates :: Ord a => [a] -> [a]
removeDuplicates = Prelude.map Prelude.head . L.group . sort

toKey v = case keyFromValues [PersistInt64 $ fromIntegral $ read v] of
    Left err ->
        error $ unpack err
    Right k ->
        k

toTimestamp = P.posixSecondsToUTCTime . realToFrac . read

checkWithinRound timestamp round = do
    Entity _ c <- activeContest
    ( start, end) <- case round of
          1 ->
              return ( contestBuildStart c, contestBuildEnd c)
          2 ->
              return ( contestBreakStart c, contestBreakEnd c)
          3 ->
              return ( contestFixStart c, contestFixEnd c)
          _ ->
              maybeFail "invalid round"
    if True || (timestamp >= start && timestamp <= end) then
        return ()
    else
        -- TODO: insert rejected??
        maybeFail "the timestamp does not fall within this round's period"

usageDispatch cmd dispatch = 
    let s = "usage: ./translator " ++ cmd ++ " " ++ ( fmap C.toUpper $ fst $ L.head dispatch) in
    L.foldl (\acc (x,_) -> acc ++ "|" ++ (fmap C.toUpper x)) s $ L.tail dispatch

printError = hPutStrLn stderr

silentFail :: MonadIO m => String -> m a
silentFail s = liftIO $ do
    hPutStrLn stderr s
    exitFailure

maybeFail :: MonadIO m => String -> m a
maybeFail s = liftIO $ do
    hPutStrLn stderr s
    putStrLn $ show $ (Nothing :: Maybe Int)
    exitFailure

boolFail :: MonadIO m => String -> m a
boolFail s = liftIO $ do
    hPutStrLn stderr s
    putStrLn $ show False
    exitFailure

instance Hashable TeamContestId where
    hashWithSalt s k' = 
        hashWithSalt s $ keyToInt k'

