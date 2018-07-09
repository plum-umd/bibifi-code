{-# LANGUAGE FunctionalDependencies, FlexibleInstances, TypeSynonymInstances #-}

module Model where

import Prelude
import Yesod
import Control.Monad.Trans.Reader
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as B64
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as Text
import Database.Persist.Sql (fromSqlKey)
import Database.LEsqueleto
import Database.LPersist
import Database.LPersist.Labeler
import Database.LPersist.TH
import Database.Persist.Quasi
-- import Database.Persist.Types
import Data.Typeable (Typeable)
import Data.Time
import LMonad.Label.DisjunctionCategory
import LMonad.TCB
import qualified Text.Blaze.Html.Renderer.Text as Blaze
import Yesod.Auth.HashDB (HashDBUser(..))
-- import Database.Persist.TH
import PostDependencyType

import Model.Internal as Model
import qualified Model.Internal as M

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/

share [mkLabels $(phantomType "DCLabel Principal"), mkLSql]
    $(lPersistFileWith lowerCaseSettings "../config/models")
    

instance HashDBUser User where
    userPasswordHash = Just . userPassword
    userPasswordSalt = Just . userSalt
    setSaltAndPasswordHash s h p = 
        p {
          userSalt = s
        , userPassword = h
        }

class ContestSubmission k where
    -- getInvolvedTeams :: ( PersistMonadBackend m ~ E.SqlBackend, MonadResource m, PersistStore m, E.MonadSqlPersist m) => k -> m [TeamContestId]
    getInvolvedTeams :: ( MonadResource m, m ~ ReaderT SqlBackend m0, MonadIO m0) => k -> m [TeamContestId]

-- instance ContestSubmission BuildSubmissionId where
--     getInvolvedTeams sId = do 
--         bsM <- E.get $ sId
--         return $ case bsM of
--             Nothing ->
--                 []
--             Just bs -> do
--                 [buildSubmissionTeam bs]
-- 
-- instance ContestSubmission BreakSubmissionId where
--     getInvolvedTeams sId = do
--         bsM <- E.get $ sId
--         return $ maybe [] (\bs -> [breakSubmissionTeam bs, breakSubmissionTargetTeam bs]) bsM
-- 

