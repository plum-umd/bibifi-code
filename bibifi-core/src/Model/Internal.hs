{-# LANGUAGE FunctionalDependencies, FlexibleInstances, TypeSynonymInstances #-}

module Model.Internal where

import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as B64
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as Text
import Data.Time
import Data.Typeable (Typeable)
import Database.Persist.Sql (fromSqlKey)
import Database.Persist.Quasi
import LMonad
import LMonad.Label.DisjunctionCategory
import Prelude
import PostDependencyType
import qualified Text.Blaze.Html.Renderer.Text as Blaze
import Yesod

share [mkPersist sqlSettings, mkMigrate "migrateAll"] 
    $(persistFileWith lowerCaseSettings "../config/models")

data Principal = 
      PrincipalUser UserId 
    | PrincipalGroup TeamId
    | PrincipalTeam TeamContestId
    | PrincipalJudge JudgeId
    | PrincipalAdmin
    deriving ( Eq, Ord, Show)

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

