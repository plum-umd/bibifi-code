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
      , optionsOracleDirectory :: FilePath
      , contest :: Entity Contest -- Unique contest url identifier
    }

data InternalOptions = InternalOptions {
        _iCount :: Maybe Int
      , _iContestUrl :: Maybe String
      , _iOracleDirectory :: Maybe String
    }

$(makeLenses ''InternalOptions)

emptyOptions :: InternalOptions
emptyOptions = InternalOptions {
        _iCount = Nothing
      , _iContestUrl = Nothing
      , _iOracleDirectory = Nothing
    }

options :: [OptDescr (InternalOptions -> IO InternalOptions)]
options = 
    let countOption = Option "t" ["thread-count"] (ReqArg (setOnceParse parseInt iCount) "COUNT") "Number of threads to run. Default is 1" in
    let urlOption = Option "c" ["contest-url"] (ReqArg (setOnce iContestUrl) "CONTEST-URL") "Contest's unique url identifier. Defaults to the default contest" in
    let oracleDirOption = Option "o" ["oracle-directory"] (ReqArg (setOnce iOracleDirectory) "ORACLE-DIRECTORY") "Directory where the oracle is located" in
    [countOption, oracleDirOption, urlOption]
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

parseOptions :: DatabaseM Options
parseOptions = do
    args' <- liftIO getArgs
    case getOpt Permute options args' of
        (opts,args,[]) -> do
            internalOpts <- liftIO $ foldl' (>>=) (return emptyOptions) opts
            toOptions internalOpts args
        (_, _, es) ->
            exitWithUsage $ concat es

toOptions :: InternalOptions -> [String] -> DatabaseM Options
toOptions (InternalOptions countM urlM oracleDirM) args = do
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
            putLog "Warning: No oracle directory specified. Assuming working directory."
            return ""
        Just oracleDir ->
            return oracleDir
    url <- case urlM of 
        Nothing -> do
            putLog "Warning: No contest specified. Assuming default contest."
            retrieveActiveContestUrl
        Just url ->
            return $ Text.pack url
    contest <- retrieveContest url
    return $ Options count repoDir oracleDir contest
