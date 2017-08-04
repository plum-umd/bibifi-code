{-# LANGUAGE FunctionalDependencies, FlexibleInstances, TypeSynonymInstances #-}

module Model where

import Prelude
import Yesod
import Control.Monad.Trans.Reader
import qualified Data.Aeson as Aeson
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

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/

share [mkPersist sqlSettings, mkMigrate "migrateAll", mkLabels "DCLabel Principal", mkLSql]
    $(lPersistFileWith lowerCaseSettings "../config/models")

data Principal = 
      PrincipalUser UserId 
    | PrincipalGroup TeamId
    | PrincipalTeam TeamContestId
    | PrincipalJudge JudgeId
    | PrincipalAdmin
    deriving ( Eq, Ord, Show)

instance ToLabel (Key User) (DCLabel Principal) where
    toConfidentialityLabel uId = dcConfidentialitySingleton $ PrincipalUser uId
    toIntegrityLabel uId = dcIntegritySingleton $ PrincipalUser uId

instance ToLabel (Key TeamContest) (DCLabel Principal) where
    toConfidentialityLabel tcId = dcConfidentialitySingleton $ PrincipalTeam tcId
    toIntegrityLabel tcId = dcIntegritySingleton $ PrincipalTeam tcId

instance ToLabel (Key Judge) (DCLabel Principal) where
    toConfidentialityLabel jId = dcConfidentialitySingleton $ PrincipalJudge jId
    toIntegrityLabel jId = dcIntegritySingleton $ PrincipalJudge jId

instance ToLabel (Key Team) (DCLabel Principal) where
    toConfidentialityLabel tId = dcConfidentialitySingleton $ PrincipalGroup tId
    toIntegrityLabel tId = dcIntegritySingleton $ PrincipalGroup tId

instance ToLabel String (DCLabel Principal) where
    toConfidentialityLabel "Admin" = dcConfidentialitySingleton PrincipalAdmin
    toConfidentialityLabel _ = error "ToLabel: Invalid string"
    toIntegrityLabel "Admin" = dcIntegritySingleton PrincipalAdmin
    toIntegrityLabel _ = error "ToLabel: Invalid string"

instance HashDBUser User where
    userPasswordHash = Just . userPassword
    userPasswordSalt = Just . userSalt
    setSaltAndPasswordHash s h p = 
        p {
          userSalt = s
        , userPassword = h
        }

instance ToJSON TeamBuildScore where
    toJSON (TeamBuildScore team build break fix timestamp) = object [
        "team" .= toJSON team,
        "buildScore" .= toJSON build,
        "breakScore" .= toJSON break,
        "fixScore" .= toJSON fix,
        "timestamp" .= toJSON timestamp
      ]

instance ToJSON TeamBreakScore where
    toJSON (TeamBreakScore team build break fix timestamp) = 
      object [
        "team" .= toJSON team,
        "buildScore" .= toJSON build,
        "breakScore" .= toJSON break,
        "fixScore" .= toJSON fix,
        "timestamp" .= toJSON timestamp
      ]

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

instance ToJSON Html where
    toJSON = Aeson.String . Text.toStrict . Blaze.renderHtml

instance FromJSON Html where
    parseJSON = Aeson.withText "Html" $ return . preEscapedToMarkup

instance ToJSON ByteString where
    toJSON = Aeson.String . Text.decodeUtf8 . B64.encode

instance FromJSON ByteString where
    parseJSON = Aeson.withText "ByteString" $ either (fail "Not base 64 encoded.") return . B64.decode . Text.encodeUtf8

instance ToJSON (Entity TeamBuildScore) where
    toJSON e = Aeson.object ["key" .= fromSqlKey ( entityKey e), "value" .= entityVal e]

instance ToJSON (Entity TeamBreakScore) where
    toJSON e = Aeson.object ["key" .= fromSqlKey ( entityKey e), "value" .= entityVal e]
