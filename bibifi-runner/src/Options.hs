{-# LANGUAGE TemplateHaskell, RankNTypes #-}

module Options (Options(..), parseOptions) where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Core
import Data.Foldable (foldl')
import qualified Data.Text as Text
import Database.Persist
import Model
import System.Console.GetOpt
import System.Environment
import Text.Read

import Common

data Options = Options {
        optionsCount :: Int
      , optionsDirectory :: FilePath -- Repo directory
      , optionsProblemDirectory :: FilePath -- Problem materials directory.
      , contest :: Entity Contest -- Unique contest url identifier
      , databaseConfig :: DatabaseConf
    }

data InternalOptions = InternalOptions {
        _iCount :: Maybe Int
      , _iContestUrl :: Maybe String
      , _iProblemDirectory :: Maybe String
      , _iDatabaseConfig :: Maybe String
    }

$(makeLenses ''InternalOptions)

emptyOptions :: InternalOptions
emptyOptions = InternalOptions {
        _iCount = Nothing
      , _iContestUrl = Nothing
      , _iProblemDirectory = Nothing
      , _iDatabaseConfig = Nothing
    }

options :: [OptDescr (InternalOptions -> IO InternalOptions)]
options = 
    let countOption = Option "t" ["thread-count"] (ReqArg (setOnceParse parseInt iCount) "COUNT") "Number of threads to run. Default is 1" in
    let urlOption = Option "c" ["contest-url"] (ReqArg (setOnce iContestUrl) "CONTEST-URL") "Contest's unique url identifier. Defaults to the default contest" in
    let problemDirOption = Option "p" ["problem-directory"] (ReqArg (setOnce iProblemDirectory) "PROBLEM-DIRECTORY") "Directory where the problem materials are located" in
    let databaseConfigOption = Option "d" ["database-config"] (ReqArg (setOnce iDatabaseConfig) "DATABASE-CONFIG") "Name of database configuration to load. Defaults to `Development`" in
    [countOption, problemDirOption, urlOption, databaseConfigOption]
    where
        setOnce :: Lens' InternalOptions (Maybe a) -> a -> InternalOptions -> IO InternalOptions
        setOnce lens arg opts = case view lens opts of
            Nothing ->
                return $ opts & lens .~ Just arg
            Just _ ->
                exitWithError "Options may only be specified once"

        setOnceParse :: (a -> IO b) -> Lens' InternalOptions (Maybe b) -> a -> InternalOptions -> IO InternalOptions
        setOnceParse parser lens arg' opts = do
            arg <- parser arg'
            setOnce lens arg opts

        parseInt :: String -> IO Int
        parseInt str = case readMaybe str of
            Nothing -> 
                exitWithError "Invalid integer"
            Just i ->
                return i

exitWithUsage :: MonadIO m => String -> m a
exitWithUsage err = exitWithError $ err ++ "\n" ++ usageInfo "Usage: runner OPTIONS [repo-directory]" options

parseOptions :: IO Options
parseOptions = do
    args' <- getArgs
    case getOpt Permute options args' of
        (opts,args,[]) -> do
            internalOpts <- foldl' (>>=) (return emptyOptions) opts
            toOptions internalOpts args
        (_, _, es) ->
            exitWithUsage $ concat es

toOptions :: InternalOptions -> [String] -> IO Options
toOptions (InternalOptions countM urlM oracleDirM dbNameM) args = do
    let count = maybe 1 id countM
    when (count < 1) $
        exitWithError "COUNT must be greater than 0"
    repoDir <- case args of
        [] -> do
            putLog "Warning: No repository directory specified. Assuming working directory."
            return ""
        [dir] -> 
            return dir
        _ -> 
            exitWithError "Only one repository directory can be specified."
    oracleDir <- case oracleDirM of
        Nothing -> do
            exitWithError "No oracle directory specified."
        Just oracleDir ->
            return oracleDir
    dbName <- case dbNameM of
        Nothing -> do
            putLog "Warning: No database configuration name provided. Defaulting to `Development`."
            return "Development"
        Just dbName ->
            return $ Text.pack dbName

    -- Load database configuration.
    db <- makeDatabaseConf productionDatabaseYML dbName
    runDatabaseM db $ do
        url <- case urlM of 
            Nothing -> do
                putLog "Warning: No contest specified. Assuming default contest."
                retrieveActiveContestUrl
            Just url ->
                return $ Text.pack url
        contest <- retrieveContest url
        return $ Options count repoDir oracleDir contest db
