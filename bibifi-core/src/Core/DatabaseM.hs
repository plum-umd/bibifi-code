{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Core.DatabaseM where

import Control.Monad.Trans.Reader
import Data.Aeson (Value)
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Yaml (decodeFile, parseMonad)
import Database.Persist
import Database.Persist.Postgresql

import Core.Database

type PersistConf = PostgresConf
type DatabaseM = ReaderT DatabaseConf IO

instance GeneralPersist DatabaseConf DatabaseM where
    type GeneralPersistBackend DatabaseConf = SqlBackend
    runDB' = runDB

instance GeneralPersistSql DatabaseConf DatabaseM

runDatabaseM :: DatabaseConf -> DatabaseM a -> IO a
runDatabaseM = flip runReaderT

data DatabaseConf = DatabaseConf {
        getPool :: Database.Persist.PersistConfigPool PersistConf
      , getConfig :: PersistConf
    }

-- runDB :: (PersistConfig c, PersistConfigBackend c ~ SqlPersistT, c ~ SqlBackend) => 
--     PersistConfigBackend c DatabaseM a -> DatabaseM a
runDB :: SqlPersistT DatabaseM b -> DatabaseM b
runDB f = do
    dbConf <- ask
    runPool (getConfig dbConf) f (getPool dbConf)

productionDatabaseYML :: FilePath
productionDatabaseYML = "../config/postgresql.yml"

makeDatabaseConf :: FilePath -> Text -> IO DatabaseConf
makeDatabaseConf databaseYML dbname = do
    dbConfJson <- justEnv dbname `fmap` loadYaml databaseYML
    dbConf <- parseMonad loadConfig dbConfJson
    pool <- createPoolConfig (dbConf :: PersistConf)
    return $ DatabaseConf pool dbConf

    where
        loadYaml :: String -> IO (Map.Map Text Value)
        loadYaml fp = do
            valM <- decodeFile fp
            case valM of
                Nothing -> 
                    error $ "Invalid YAML file: " ++ show fp
                Just o -> 
                    return o
        -- https://github.com/yesodweb/yesod/wiki/Using-Database.Persist.runPool-without-Foundation
        justEnv envName obj = 
          case Map.lookup envName obj of
              Nothing -> error "Could not find environment"
              Just env -> env
        
