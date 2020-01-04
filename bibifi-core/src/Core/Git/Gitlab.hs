module Core.Git.Gitlab where

import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Data.Aeson (FromJSON(..), Value(..), (.:))

import GitLab (GitLabServerConfig(..))
import qualified GitLab

instance FromJSON GitLabServerConfig where
    parseJSON (Object y) = do
        url <- y .: "url"
        token <- y .: "token"
        return $ GitLab.defaultGitLabServer {url = url, token = token}
    parseJSON _ = 
        fail "Invalid GitLabServerConfig JSON"

getFileArchive :: (MonadUnliftIO m, MonadIO m) => GitLabServerConfig -> Int -> FilePath -> m ()
getFileArchive gitConfig repoId file = GitLab.runGitLab gitConfig $ GitLab.getFileArchive' repoId GitLab.TarGz file
