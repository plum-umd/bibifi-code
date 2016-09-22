module Cloud.Docker (launchOneDockerWithTimeout) where

import Control.Monad.Error
import Control.Monad.Trans.Resource
import qualified Data.Text as Text
import qualified Docker.Client as Docker
import Docker.Client.Http (runDockerT)
-- import qualified Docker.Client.Types as Docker
import qualified Network.HTTP.Conduit as HTTP
import Network.SSH.Client.SimpleSSH
import Text.Read (readMaybe)

import Common
import Cloud.Internal
import Core.SSH

launchOneDockerWithTimeout :: (MonadBaseControl IO m, MonadIO m, MonadThrow m, BackendError e) => DockerConfiguration -> HTTP.Manager -> Int -> (CloudInstance -> Session -> ErrorT e (CloudT m) b) -> ErrorT e m b
launchOneDockerWithTimeout conf manager timer f = ErrorT $ 
    -- Create and start docker.
    runDockerT (clientOpts, handler) $ withCreateContainer $ withStartContainer $ \containerId -> do
        -- Find ip and port.
        (ip, port) <- getIPAndPort containerId

        -- Connect via SSH.
        lift $ runCloudT $ withSSH ip port keyFiles timer $ f (CloudDockerInstance containerId)

    where

        getIPAndPort containerId = do
            let containerIdS = show containerId
            infoE <- Docker.inspectContainer containerId
            case infoE of
                Left err -> 
                    fail $ strMsg $ "Could not get container information (" ++ containerIdS ++ "): " ++ show err
                Right info ->
                    let ns = Docker.networkSettings info in
                    case Docker.networkSettingsPorts ns of
                        [(Docker.PortBinding _ _ [(Docker.HostPort ip' port)])] -> case readMaybe $ Text.unpack ip' of
                            Nothing ->
                                fail $ strMsg $ "Invalid IP returned: " ++ containerIdS
                            Just ip ->
                                return (ip, port)
                        _ -> 
                            fail $ strMsg $ "Container does not have a single port: " ++ containerIdS

        keyFiles = KeyFiles (dockerSSHPrivateKeyFile conf) (dockerSSHPublicKeyFile conf)

        -- handler :: Docker.HttpHandler (CloudT m)
        handler = Docker.httpHandler manager

        clientOpts = Docker.defaultClientOpts {Docker.baseUrl = Text.pack $ dockerUrl conf}
        
        startOpts = Docker.defaultStartOpts
            -- Docker.StartContainerOpts [] [] [] [] True False [] [] Docker.RestartNever

        containerOpts = 
            let cont = Docker.defaultContainerConfig (dockerImageId conf) in
            let res = Docker.defaultContainerResources { 
                    Docker.memory = Just $ Docker.MemoryConstraint (dockerMemory conf) Docker.GB
                  , Docker.cpuShares = Just (dockerCpu conf)
                  }
            in
            let host = Docker.defaultHostConfig { 
                    Docker.resources = res
                  , Docker.publishAllPorts = True
                  } 
                  -- TODO: Expose ports? XXX
            in
            Docker.CreateOpts cont host
            
        leaveRunning = dockerLeaveRunning conf

        -- withCreateContainer :: (ContainerID -> DockerT (ErrorT e m) a) -> DockerT (ErrorT e m) a
        withCreateContainer f = do
            containerIdM <- Docker.createContainer containerOpts Nothing
            case containerIdM of
                Left err ->
                    return $ Left $ strMsg $ "Could not create container (" ++ show err ++ ")."
                Right containerId -> do
                    res <- f containerId

                    -- Delete container.
                    unless leaveRunning $ do
                        let deleteOpts = Docker.DeleteOpts True True
                        stat <- Docker.deleteContainer deleteOpts containerId
                        case stat of
                            Left err -> 
                                putLog $ "Could not delete container: " ++ show containerId ++ "(" ++ show err ++ ")"
                            Right () ->
                                return ()

                    return res

        -- withStartContainer :: MonadIO m => (ContainerID -> DockerT (ErrorT e m) a) -> ContainerID -> DockerT (ErrorT e m) a
        withStartContainer f containerId = do
            stat <- Docker.startContainer startOpts containerId
            case stat of
                Left err ->
                    return $ Left $ strMsg $ "Could not start container: " ++ show containerId ++ "(" ++ show err ++ ")"
                Right () -> do
                    res <- f containerId

                    -- Shutdown container.
                    if not leaveRunning then do
                        stat <- Docker.killContainer Docker.SIGKILL containerId
                        case stat of
                            Left err ->
                                putLog $ "Could not stop container: " ++ show containerId ++ "(" ++ show err ++ ")"
                            Right () ->
                                return ()
                    else
                        putLog $ "Leaving container running: " ++ show containerId

                    return res

