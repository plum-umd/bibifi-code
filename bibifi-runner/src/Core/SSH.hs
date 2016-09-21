module Core.SSH where

import Control.Monad.Error
import Control.Monad.Trans.Control
import qualified Data.ByteString as BS
import Data.IP
import Data.Monoid
import Network.SSH.Client.SimpleSSH
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import qualified System.IO.Temp as Temp
import System.Timeout.Lifted

import Common

createNewUser :: MonadIO m => Session -> String -> ErrorT String m ()
createNewUser session user = do
    (Result _ _ exit) <- runSSH "Could not create user" $ execCommand session $ "sudo adduser --disabled-password --gecos \"\" " <> user
    when (exit /= ExitSuccess) $
        fail $ "Could not create user: " <> user

-- Run a command via SSH. 
runSSH :: MonadIO m => e -> SimpleSSH a -> ErrorT e m a
runSSH err m = ErrorT $ do
    resE <- liftIO $ runSimpleSSH m
    case resE of
        Left e -> do
            putLog $ show e
            return $ Left err
        Right res ->
            return $ Right res

uploadString :: (MonadIO m, Error e) => Session -> BS.ByteString -> String -> ErrorT e m ()
uploadString session contents targetFile = do
    tmpFile <- liftIO $ do
        tmp <- Directory.getTemporaryDirectory
        tmpDir <- Temp.createTempDirectory tmp "bibifi"
        return $ FilePath.combine tmpDir "args"
    liftIO $ BS.writeFile tmpFile contents
    let err = "Could not send file: " <> tmpFile
    _ <- runSSH (strMsg err) $ sendFile session 0o666 tmpFile targetFile
    removeIfExists tmpFile
    return ()

-- Connect via SSH to an instance. 
openSSH :: MonadIO m => IPv4 -> Integer -> KeyFiles -> m (Either String Session)
openSSH ip' port keyFiles = do
    -- Attempt to connect to instance.
    let ip = show ip'
    result <- liftIO $ runSimpleSSH $ openSession' ip port Nothing -- "~/.ssh/known_hosts"
    case result of
        Left _err -> do
            putLog $ show _err

            -- SSH server not running yet, so sleep and try again. 
            liftIO $ threadDelay 5000000

            openSSH ip' port keyFiles
        Right session -> do
            let pubFile = keyFilesPublicKey keyFiles
            let privFile = keyFilesPrivateKey keyFiles
            result' <- liftIO $ runSimpleSSH $ authenticateWithKey session "ubuntu" pubFile privFile ""
            case result' of
                Left e ->
                    return $ Left $ "Authentication failed for ip (" ++ show e ++ "): " ++ ip
                Right r ->
                    return $ Right r

withSSH :: (MonadIO m, MonadBaseControl IO m, BackendError e) =>
    IPv4 -> Integer -> KeyFiles -> Int -> (Session -> ErrorT e m b) -> m (Either e b)
withSSH ip port keyFiles timer f = do
    -- Connect via SSH.
    putLog "Waiting for SSH connection."
    sessionE <- openSSH ip port keyFiles
    case sessionE of
        Left err -> do
            -- Could not log into instance.
            return $ Left $ strMsg err
        Right session -> do
            putLog "SSH connection established."

            -- Start timeout and run f. 
            let timer' = timer * 60 * 1000000
            res <- timeout timer' $ 
                runErrorT $ f session

            -- Close session.
            _ <- liftIO $ runSimpleSSH $ closeSession session

            case res of 
                Nothing ->
                    return $ Left backendTimeout
                Just r ->
                    return r

setupFirewall :: (MonadIO m, Error e) => Session -> ErrorT e m ()
setupFirewall session = do
    (Result _ _ exit) <- runSSH (strMsg "Could not setup firewall") $ execCommand session "sudo iptables -A OUTPUT -m state --state RELATED,ESTABLISHED -j ACCEPT && sudo iptables -A OUTPUT -p tcp --sport 22 -j ACCEPT && sudo iptables -A INPUT -i lo -j ACCEPT && sudo iptables -A OUTPUT -o lo -j ACCEPT && sudo iptables -A OUTPUT -j DROP"
    when (exit /= ExitSuccess) $
        throwError $ strMsg "Could not setup firewall"

