module Yesod.Auth.OAuth2.Coursera where

import Control.Exception
import Control.Exception.Enclosed
import Control.Monad
import Data.Aeson (Value(..), FromJSON(..), (.:), (.:?))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Data.Vector ((!?))
import Network.HTTP.Client (Manager)
import Prelude
import Yesod.Auth
import Yesod.Auth.OAuth2

data CourseraProfile = CourseraProfile {
        courseraId :: Text
      , courseraLocale :: Maybe Text
      , courseraTimezone :: Maybe Text
    }
    deriving (Show)

instance FromJSON CourseraProfile where
    parseJSON (Object o) = do
        elems <- o .: "elements"
        case elems of
            (Array es) -> case (es !? 0) of
                Just (Object elem) -> do
                    cId <- elem .: "id"
                    locale <- elem .:? "locale"
                    timezone <- elem .:? "timezone"
                    return $ CourseraProfile cId locale timezone
                _ ->
                    mzero
            _ -> 
                mzero
    parseJSON _ = mzero

-- oauth2Callback :: Text -> AuthRoute
-- oauth2Callback name = PluginR name ["callback"]

oauth2CourseraUrl :: AuthRoute
oauth2CourseraUrl = PluginR "coursera" ["forward"]

oauth2Coursera :: YesodAuth m => BS.ByteString -> BS.ByteString -> [BS.ByteString] -> AuthPlugin m
oauth2Coursera clientId clientSecret scopes = do
    -- csrfToken <- generateToken
    let params = [("scope", BS.intercalate "%20" scopes)]
    -- let params = [("scope", BS.intercalate "%20" scopes),("redirect_uri", oauth2Callback "Coursera")] in
    let uri = appendQueryParam "https://accounts.coursera.org/oauth2/v1/auth" params
    authOAuth2 "coursera" (OAuth2
        clientId
        clientSecret
        uri
        "https://accounts.coursera.org/oauth2/v1/token" -- accessTokenEndpoint
        Nothing -- oauthCallback
      ) $ \manager token -> do
        let params = [("q","me"),("fields","timezone,locale,privacy")]
        let uri = appendQueryParam "https://api.coursera.org/api/externalBasicProfiles.v1" params
        resultE <- authGetJSON' manager token uri
        case resultE of
            Left e -> 
                throwIO $ InvalidProfileResponse "coursera" e
            Right res -> 
                let extras'' = [("token", Text.decodeUtf8 $ accessToken token)] in
                let extras' = maybe extras'' (\e -> ("timezone",e):extras'') $ courseraTimezone res in
                let extras = maybe extras'' (\e -> ("locale",e):extras') $ courseraLocale res in
                return $ Creds "coursera" (courseraId res) extras

-- {"enrollments":[{"id":97513801,"sessionId":973457,"isSigTrack":false,"courseId":219,"active":true,"startDate":1413158400,"endDate":1416787200,"startStatus":"Past"},{"id":97513875,"sessionId":973097,"isSigTrack":false,"courseId":1517,"active":true,"startDate":1412294400,"endDate":1414972800,"startStatus":"Past"}],"courses":[{"id":219,"name":"Introduction to Guitar","shortName":"guitar","photo":"https://coursera-course-photos.s3.amazonaws.com/8f/d4aece023b1764926a3c9adb680b0d/guitar-large.jpg","smallIconHover":"https://d15cw65ipctsrr.cloudfront.net/ee/e6e3cb2cd2ea22f792fd5126d98f7b/guitar-large.jpg"},{"id":1517,"name":"Learning How to Learn: Powerful mental tools to help you master tough subjects","shortName":"learning","photo":"https://coursera-course-photos.s3.amazonaws.com/6e/c02c90d08611e3bb7b4ba94dd73d39/Learning-How-to-Learn-Logo-with-text.png","smallIconHover":"https://d15cw65ipctsrr.cloudfront.net/6f/a387b0d08611e3809f573e2bc8ee21/Learning-How-to-Learn-Logo-with-text.png"}]}
oauth2CourseraEnrollments :: Manager -> Text -> IO (OAuth2Result Enrollments)
oauth2CourseraEnrollments manager token = do
    -- TODO: Do we need to add a refresh???
    authGetJSON' manager (AccessToken (Text.encodeUtf8 token) Nothing Nothing Nothing Nothing) "https://api.coursera.org/api/users/v1/me/enrollments"

data Enrollment = Enrollment {
--        enrollmentId :: Int
        enrollmentCourseId :: Int
      , enrollmentSessionId :: Int
    }
newtype Enrollments = Enrollments [Enrollment]

instance FromJSON Enrollments where
    parseJSON (Object o) = do
        enrollments <- o .: "enrollments"
        return $ Enrollments enrollments
    parseJSON _ = mzero

instance FromJSON Enrollment where
    parseJSON (Object o) = do
        -- i <- o .: "id"
        courseId <- o .: "courseId"
        sessionId <- o .: "sessionId"
        return $ Enrollment courseId sessionId
    parseJSON _ = mzero

authGetJSON' manager token url = catchAny (authGetJSON manager token url) handler
    where
        handler e = return $ Left $ BSL8.pack $ show e
