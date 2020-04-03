module Core.Git where

import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import qualified Data.Aeson.TH as Aeson
import Data.Yaml
import Database.Persist.TH
import GitLab (GitLabServerConfig)

import qualified Core.Git.Gitlab as Gitlab

-- JP: In the future, we could support multiple git sites. (Map DomainName GitConfig)
data GitConfiguration = GitlabConfiguration GitLabServerConfig

loadGitConfiguration :: String -> IO (Either String GitConfiguration)
loadGitConfiguration configFile = do
    -- Read config file.
    yamlE <- decodeFileEither configFile
    case yamlE of
        Left err ->
            return $ Left $ "File '" ++ configFile ++ "' is invalid or it doesn't exist: " ++ show err
        Right yaml ->
            return $ Right yaml

instance FromJSON GitConfiguration where
    parseJSON (Object y) = do
        gitlabM <- y .:? "Gitlab"
        case gitlabM of 
            Just gitlab ->
                return $ GitlabConfiguration gitlab
            Nothing ->
                fail "Invalid GitConfiguration JSON"
    parseJSON _ = 
        fail "Invalid GitConfiguration JSON"

data RepositoryIdentifier = GitlabRepositoryIdentifier Int
    deriving (Show, Read)
derivePersistField "RepositoryIdentifier"
Aeson.deriveJSON Aeson.defaultOptions ''RepositoryIdentifier

getFileArchive ::  (MonadUnliftIO m, MonadIO m) => GitConfiguration -> RepositoryIdentifier -> FilePath -> m ()
getFileArchive (GitlabConfiguration gitConfig) (GitlabRepositoryIdentifier repoId) file = Gitlab.getFileArchive gitConfig repoId file

