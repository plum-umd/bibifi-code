module Git (
    module Git
  , loadGitConfiguration
  ) where

import Common
import qualified Core.Git 
import Core.Git as Git hiding (loadGitConfiguration)

loadGitConfiguration :: String -> IO GitConfiguration
loadGitConfiguration configFile = do
    confE <- Core.Git.loadGitConfiguration configFile
    case confE of
        Left err ->
            exitWithError err
        Right conf ->
            return conf

