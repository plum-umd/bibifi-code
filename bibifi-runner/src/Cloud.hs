-- | Abstraction of cloud backends. Supports EC2 and Docker Swarm.
module Cloud (
      launchOneInstanceWithTimeout
    , productionCloudYML
    , cloudManagerSettings
    , loadCloudConfiguration
    , module Export
    ) where

import Cloud.EC2
import Cloud.Docker
import Cloud.Internal as Export
import Control.Monad.Trans.Resource
import Control.Monad.Error
import Data.Yaml
import Docker.Client
import qualified Network.HTTP.Conduit as HTTP
import Network.Connection (TLSSettings(..))
import Network.SSH.Client.SimpleSSH
-- import Network.URI (parseURI, URI(..), URIAuth(..))
-- import Text.Read (readMaybe)

import Common

launchOneInstanceWithTimeout :: (MonadBaseControl IO m, MonadIO m, MonadThrow m, BackendError e) =>
    CloudConfiguration -> HTTP.Manager -> Int -> (CloudInstance -> Session -> ErrorT e (CloudT m) b) -> ErrorT e m b
launchOneInstanceWithTimeout (CloudEC2Configuration ec2) manager timer f = launchOneEC2WithTimeout'' ec2 manager timer f
launchOneInstanceWithTimeout (CloudDockerConfiguration conf) manager timer f = launchOneDockerWithTimeout conf manager timer f

productionCloudYML :: String
productionCloudYML = "/fs/mc2-application/config/cloud.yml"

loadCloudConfiguration :: String -> IO CloudConfiguration
loadCloudConfiguration configFile = do
    -- Read 'config/aws.yml'
    yamlE <- decodeFileEither configFile
    case yamlE of
        Left err -> 
            exitWithError $ "File '" ++ configFile ++ "' is invalid or it doesn't exist: " ++ show err
        Right yaml ->
            return yaml

cloudManagerSettings :: CloudConfiguration -> IO HTTP.ManagerSettings
cloudManagerSettings (CloudEC2Configuration _) = return HTTP.conduitManagerSettings
cloudManagerSettings (CloudDockerConfiguration cfg) = do
    let host = dockerAddress cfg
    let port = fromInteger $ dockerPort cfg
    paramsE <- clientParamsWithClientAuthentication host port privKey cert
    case paramsE of
        Left err -> 
            exitWithError $ "Could not load client authentication: " ++ err
        Right params' -> do
            params <- clientParamsSetCA params' $ dockerCAFile cfg
            let settings = TLSSettings params
            return $ HTTP.mkManagerSettings settings Nothing
    
    where
        -- url = dockerUrl cfg
        privKey = dockerPrivateKeyFile cfg
        cert = dockerCertificateFile cfg
    

