{-# LANGUAGE OverloadedStrings, TypeFamilies, UndecidableInstances #-}
module Cloud.Internal where

-- import qualified Cloud.AWS as AWS
-- import qualified Cloud.AWS.EC2.Types as AWS
-- import Cloud.AWS.Class
-- -- import Cloud.AWS.EC2
-- -- import Cloud.AWS.EC2.Types
import Control.Monad.Base
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Control
-- import Control.Monad.Trans.Resource
import Data.Aeson (FromJSON(..), (.:?), Value(..), (.:))
import Data.Text (Text)
-- import qualified Data.Text.Encoding as Text
import qualified Docker.Client.Types as Docker

data CloudT m a = CloudT { 
      runCloudT :: !(m a)
    }
--      CloudTAWS (AWS AWSContext (ResourceT m) a)
--    | CloudTDocker

instance (Functor m) => Functor (CloudT m) where
    fmap f (CloudT m) = CloudT $ fmap f m

instance (Applicative m, Functor m) => Applicative (CloudT m) where
    pure = CloudT . pure
    (CloudT f) <*> (CloudT m) = CloudT $ f <*> m

instance (Monad m) => Monad (CloudT m) where
    (CloudT m) >>= f = CloudT $ m >>= runCloudT . f
    return = CloudT . return

instance (MonadIO m) => MonadIO (CloudT m) where
    liftIO = lift . liftIO

instance MonadTrans CloudT where
    lift = CloudT

instance (MonadBase IO m) => MonadBase IO (CloudT m) where
    liftBase = lift . liftBase

instance MonadTransControl CloudT where
    type StT CloudT a = a
    liftWith f = CloudT $ f runCloudT
    restoreT = CloudT

instance MonadBaseControl IO m => MonadBaseControl IO (CloudT m) where
    type StM (CloudT m) a = ComposeSt CloudT m a
    liftBaseWith = defaultLiftBaseWith
    restoreM = defaultRestoreM

data CloudInstance = 
--       CloudEC2Instance AWS.Instance
      CloudDockerInstance Docker.ContainerID

data CloudConfiguration = 
--       CloudEC2Configuration EC2Configuration
      CloudDockerConfiguration DockerConfiguration

instance FromJSON CloudConfiguration where
    parseJSON (Object y) = do
        dockerM <- y .:? "Docker"
        case dockerM of
            Just docker ->
                return $ CloudDockerConfiguration docker
            Nothing -> do
                -- ec2M <- y .:? "EC2"
                -- case ec2M of
                --     Just ec2 ->
                --         return $ CloudEC2Configuration ec2
                --     Nothing -> 
                        fail "Invalid CloudConfiguration JSON"
    parseJSON _ = 
        fail "Invalid CloudConfiguration JSON"


-- EC2 functionality.

-- data EC2Configuration = EC2Configuration {
--         ec2Credential :: AWS.Credential
--       , ec2ImageId :: Text
--       , ec2KeyName :: Text
--       , ec2PrivateKeyFile :: String
--       , ec2PublicKeyFile :: String
--       , ec2SecurityGroup :: Text
--       , ec2LeaveRunning :: Bool
--       , ec2InstanceType :: Maybe Text
--     }
-- 
-- instance FromJSON EC2Configuration where
--     parseJSON (Object e) = do
--         access <- e .: "accessKey"
--         secret <- e .: "secretKey"
--         let credential = AWS.newCredential (Text.encodeUtf8 access) (Text.encodeUtf8 secret)
--         imageId <- e .: "imageId"
--         keyName <- e .: "keyName"
--         securityGroup <- e .: "securityGroup"
--         leaveRunning <- e .: "leaveRunning"
--         privKeyFile <- e .: "privateKeyFile"
--         pubKeyFile <- e .: "publicKeyFile"
--         instanceType <- e .:? "instanceType"
--         return $ EC2Configuration credential imageId keyName privKeyFile pubKeyFile securityGroup leaveRunning instanceType
--     parseJSON _ = 
--         fail "Invalid EC2Configuration JSON"

-- Docker functionality.

data DockerConfiguration = DockerConfiguration {
      dockerCertificateFile :: String
    , dockerPrivateKeyFile :: String
    , dockerCAFile :: String
    , dockerImageId :: Text
    , dockerAddress :: String
    , dockerPort :: Integer
    , dockerLeaveRunning :: Bool
    , dockerSSHPrivateKeyFile :: String
    , dockerSSHPublicKeyFile :: String
    , dockerMemory :: Integer
    , dockerCpu :: Integer
}

dockerUrl :: DockerConfiguration -> String
dockerUrl cfg = "https://" ++ dockerAddress cfg ++ ":" ++ show (dockerPort cfg) ++ "/"

instance FromJSON DockerConfiguration where
    parseJSON (Object o) = do
        certificate <- o .: "certificate"
        privateKey <- o .: "secretKey"
        imageId <- o .: "imageId"
        address <- o .: "address"
        port <- o .: "port"
        leaveRunning <- o .: "leaveRunning"
        privKeyFile <- o .: "privateKeyFile"
        pubKeyFile <- o .: "publicKeyFile"
        memory <- o .: "memory"
        cpu <- o .: "cpu"
        ca <- o .: "caCertificate"
        return $ DockerConfiguration certificate privateKey ca imageId address port leaveRunning privKeyFile pubKeyFile memory cpu


    parseJSON _ = 
        fail "Invalid DockerConfiguration JSON"

