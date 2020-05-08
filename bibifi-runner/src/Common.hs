{-# LANGUAGE ScopedTypeVariables #-}
module Common (
        module Export
      , putLog
      , exitWithError
      , Job(..)
      , lockTeam
      , unlockTeam
      , LockSet(..)
      , acquireFileLock
      , releaseFileLock
      , pushQueue
      , popQueue
      , runComputation
      , whenJust
      , safeReadFileLazy
      , removeIfExists
      , BackendError(..)
      , KeyFiles(..)
    ) where

import Control.Concurrent as Export
import Control.Exception.Enclosed
import Control.Exception.Lifted
import Control.Monad.Error
import Control.Monad.IO.Class as Export
import Control.Monad.Trans.Control
import Core.DatabaseM as Export
-- import qualified Data.List as List
import qualified Data.ByteString.Lazy as BSL
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Database.Persist as Export
import Model as Export hiding (Error)
import PostDependencyType as Export
import System.Directory (removeFile)
import System.Exit
import System.IO
import System.IO.Error (isDoesNotExistError)

import Queue

putLog :: MonadIO m => String -> m ()
putLog message = do
    liftIO $ hPutStrLn stderr message
    liftIO $ hFlush stdout

exitWithError :: MonadIO m => String -> m a
exitWithError message = liftIO $ do
    putLog message
    exitWith $ ExitFailure 1

data Job = 
        OracleJob (Entity OracleSubmission)
      | BuildJob (Entity BuildSubmission)
      | BreakJob (Entity BreakSubmission)
      | FixJob (Entity FixSubmission)

lockTeam :: MonadIO m => TeamContestId -> MVar (Set TeamContestId) -> m ()
lockTeam = updateMVar . Set.insert

unlockTeam :: MonadIO m => TeamContestId -> MVar (Set TeamContestId) -> m ()
unlockTeam = updateMVar . Set.delete
    

data LockSet a = LockSet (MVar (Map a (MVar (), Int)))

acquireFileLock :: MonadIO m => FilePath -> LockSet FilePath -> m ()
acquireFileLock f (LockSet ls) = liftIO $ do
    -- Get (and possible create) lock for file.
    l <- modifyMVar ls $ \m -> case Map.lookup f m of
        Nothing -> do
            -- Create and insert lock.
            l <- newMVar ()
            
            -- Insert lock and update count.
            let m' = Map.insert f (l, 1) m

            return (m', l)
        Just (l, c) ->
            -- Update count.
            let m' = Map.insert f (l, c+1) m in

            return (m', l)

    -- Block on lock acquire.
    takeMVar l

releaseFileLock :: MonadIO m => FilePath -> LockSet FilePath -> m ()
releaseFileLock f (LockSet ls) = liftIO $ modifyMVar_ ls $ \m -> case Map.lookup f m of
    Nothing ->
        error "releaseFileLock: Error: file lock released too many times."
    Just (_, 1) ->
        return $ Map.delete f m
    Just (l, c) -> do
        putMVar l ()
        return $ Map.insert f (l, c-1) m
    
updateMVar :: MonadIO m => (a -> a) -> MVar a -> m ()
updateMVar f mvar = liftIO $ modifyMVar_ mvar $ return . f

pushQueue :: MonadIO m => a -> MVar (Queue a) -> m ()
pushQueue e = updateMVar $ flip enqueue e

popQueue :: MonadIO m => MVar (Queue a) -> m (Maybe a)
popQueue mvar = do
    queueM <- liftIO $ tryTakeMVar mvar
    case queueM of
        Nothing ->
            return Nothing
        Just queue -> do
            let (head, queue') = dequeue queue
            liftIO $ putMVar mvar queue'
            return head

-- Run a computation, and catch any exceptions. 
runComputation :: (MonadBaseControl IO m, MonadIO m) => a -> m a -> m a
runComputation def f = catchAny f handler
    where
        -- handler :: (MonadBaseControl IO m, MonadIO m) => SomeException -> m a
        -- handler (e :: SomeException) | List.isInfixOf "<<timeout>>" (show e) = putLog "**** HERE ****" >> throw e
        handler (e :: SomeException) = do
            putLog $ "Caught exception: " ++ show e
            return def

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust (Just a) f = f a
whenJust Nothing _ = return ()

safeReadFileLazy :: (MonadIO m) => FilePath -> m (Either String BSL.ByteString)
safeReadFileLazy file = liftIO $
    catchAny 
        (fmap Right $ BSL.readFile file) 
        $ \e -> return $ Left $ "safeReadFileLazy: " ++ show e

removeIfExists :: (MonadIO m) => FilePath -> m ()
removeIfExists fileName = liftIO $ removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = putLog $ show e

class (Error e) => BackendError e where
    backendTimeout :: e
    rejectionWithMessage :: String -> e

data KeyFiles = KeyFiles {
      keyFilesPrivateKey :: String
    , keyFilesPublicKey :: String
    }

