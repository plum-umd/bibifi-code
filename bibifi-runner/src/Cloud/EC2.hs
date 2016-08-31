{-# LANGUAGE OverloadedStrings #-}
module Cloud.EC2 (
      EC2Configuration(..)
    , launchEC2
    , launchOneEC2WithTimeout
    , launchOneEC2WithTimeout'
    , launchOneEC2WithTimeout''
    , BackendError(..)
    ) where

import Cloud.AWS
import Cloud.AWS.Class
import Cloud.AWS.EC2
import Cloud.AWS.EC2.Types
import Control.Monad.Error
import Control.Monad.Trans.Resource
import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.List as List
import qualified Network.HTTP.Conduit as HTTP
import Network.SSH.Client.SimpleSSH
import System.Timeout.Lifted

import Common
import Cloud.Internal
import Core.SSH

-- | Similar to launchOneEC2WithTimeout, except run in an ErrorT monad transformer. 
-- DEPRECATED
launchOneEC2WithTimeout' :: (MonadBaseControl IO m, MonadIO m, MonadThrow m) =>
    EC2Configuration -> HTTP.Manager -> Int -> (String -> IO (Either e b)) -> (Instance -> Session -> ErrorT e (AWS AWSContext (ResourceT m)) b) -> ErrorT e m (Maybe b)
launchOneEC2WithTimeout' ec2 manager timer e f = ErrorT $ do
    resM <- launchOneEC2WithTimeout ec2 manager timer e $ \i s -> do
        runErrorT $ f i s
    case resM of
        Nothing ->
            return $ Right Nothing
        Just (Left e) ->
            return $ Left e
        Just (Right r) ->
            return $ Right $ Just r

toKeyFiles :: EC2Configuration -> KeyFiles
toKeyFiles ec2 = KeyFiles (ec2PrivateKeyFile ec2) (ec2PublicKeyFile ec2)

-- | Use this implementation!!! The others should be deprecated. 
launchOneEC2WithTimeout'' :: (MonadBaseControl IO m, MonadIO m, MonadThrow m, BackendError e) =>
    EC2Configuration -> HTTP.Manager -> Int -> (CloudInstance -> Session -> ErrorT e (CloudT m) b) -> ErrorT e m b
launchOneEC2WithTimeout'' ec2 manager timer f = ErrorT $ do
    launchEC2 ec2 manager 1 $ \instances -> case instances of
        [inst@(Instance{instanceIpAddress = Just ip})] -> do
            lift $ lift $ runCloudT $ 
                withSSH ip 22 (toKeyFiles ec2) timer $ f (CloudEC2Instance inst)

--             -- Connect via SSH.
--             putLog "Waiting for SSH connection."
--             sessionE <- openSSH ip 22 $ toKeyFiles ec2
--             case sessionE of
--                 Left err -> do
--                     -- Could not log into instance.
--                     return $ Left $ strMsg err
--                 Right session -> do
--                     putLog "SSH connection established."
-- 
--                     -- Start timeout and run f. 
--                     let timer' = timer * 60 * 1000000
--                     res <- timeout timer' $ 
--                         lift $ lift $ runCloudT $ runErrorT $ f (CloudEC2Instance inst) session
--                         -- runErrorT $ f inst session
-- 
--                     -- Close session.
--                     _ <- liftIO $ runSimpleSSH $ closeSession session
-- 
--                     case res of 
--                         Nothing ->
--                             return $ Left backendTimeout
--                         Just r ->
--                             return r
        _ ->
            return $ Left $ strMsg "Invalid instances. More that one instance started."
    -- resM <- launchOneEC2WithTimeout ec2 manager timer (return . Left . strMsg) $ \i s -> do
    --     runErrorT $ f i s
    -- case resM of 
    --     Nothing ->
    --         return $ Left backendTimeout
    --     Just r ->
    --         return $ Right r

-- | Launch a single EC2 instance with a specified timeout (in minutes) and SSH in. 
-- DEPRECATED
launchOneEC2WithTimeout :: (MonadBaseControl IO m, MonadIO m, MonadThrow m) =>
    EC2Configuration -> HTTP.Manager -> Int -> (String -> IO b) -> (Instance -> Session -> AWS AWSContext (ResourceT m) b) -> m (Maybe b)
launchOneEC2WithTimeout ec2 manager timer e f = 
    launchEC2 ec2 manager 1 $ \instances -> case instances of
        [inst@(Instance{instanceIpAddress = Just ip})] -> do
            -- Connect via SSH.
            putLog "Waiting for SSH connection."
            sessionE <- openSSH ip 22 $ toKeyFiles ec2
            case sessionE of
                Left err -> do
                    -- Could not log into instance.
                    liftIO $ fmap Just $ e err
                Right session -> do
                    putLog "SSH connection established."

                    -- Start timeout and run f. 
                    let timer' = timer * 60 * 1000000
                    res <- timeout timer' $ 
                        f inst session

                    -- Close session.
                    _ <- liftIO $ runSimpleSSH $ closeSession session

                    return res
        _ ->
            liftIO $ fmap Just $ e "Invalid instances. More that one instance started."

launchEC2 :: (MonadBaseControl IO m, MonadIO m, MonadThrow m) => EC2Configuration -> HTTP.Manager -> Int -> ([Instance] -> EC2 (ResourceT m) b) -> m b
launchEC2 ec2 manager count f = do
    -- Run EC2 monad.
    let credential = ec2Credential ec2
    putLog "Creating EC2 client."
    runResourceT $ runEC2withManager' credential manager $ do
        -- Set the region and endpoint.
        setRegion "us-east-1"
        setEndpoint "ec2.us-east-1.amazonaws.com"

        -- Create instance request.
        putLog "Submitting run instance request."
        let instanceRequest = (defaultRunInstancesRequest (ec2ImageId ec2) count count) {runInstancesRequestKeyName = Just (ec2KeyName ec2), runInstancesRequestSecurityGroups = [ec2SecurityGroup ec2], runInstancesRequestInstanceType = ec2InstanceType ec2}
        reservation' <- runInstances instanceRequest

        -- Wait until instances are ready. 
        instances <- waitUntilRunning [reservation']

        putLog "Instances are running."

        result <- f instances

        -- Check if we should shutdown the instances.
        let instancesS = List.intersperse ", " $ map (show . instanceId) instances
        if ec2LeaveRunning ec2 then
            putLog $ List.concat $ "Leaving instances running: " : instancesS
        else do
            putLog $ List.concat $ "Terminating instances: " : instancesS
            stateChange <- terminateInstances $ map instanceId instances
            _ <- lift $ stateChange $$+- CL.consume
            return ()

        return result

    where
        -- TODO: Add timeout after a minute (12 tries)?
        waitUntilRunning reservations =
            let instances = List.concat $ map reservationInstanceSet reservations in
            if List.all (\i -> instanceState i == InstanceStateRunning) instances then
                return instances
            else do
                -- Sleep 5 seconds.
                liftIO $ threadDelay 5000000

                -- putLog $ show instances

                -- Request current status.
                response <- describeInstances (map instanceId instances) []
                reservation' <- lift $ response $$+- CL.consume
                waitUntilRunning reservation'


-- TODO: These probably belong in ec2 libraries. 
runEC2withManager' :: Monad m => Credential -> HTTP.Manager -> AWS AWSContext m a -> m a
runEC2withManager' credentials manager = 
    runAWSwithManager manager initialEC2Context $ defaultSettings credentials

initialEC2Context :: HTTP.Manager -> AWSContext
initialEC2Context mgr = AWSContext
    { manager = mgr
    , endpoint = "ec2.amazonaws.com"
    , lastRequestId = Nothing
    }

