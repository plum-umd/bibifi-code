module Dump where

import Control.Monad.Trans.Class (lift)
import Crypto.Hash.SHA256 (hash)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.Char as C
import Data.Hex (hex)
import qualified Data.Text.Encoding as Text
import Database.Persist

import Common

dump :: [String] -> DatabaseM ()
dump [anon] | fmap C.toLower anon == "anonymize" = do
    -- Anonymize and dump database.
    user <- fmap (Aeson.toJSON . fmap anonymizeUser) $ runDB $ selectList ([] :: [Filter User]) []
    userInformation <- fmap (Aeson.toJSON . fmap anonymizeUserInformation) $ runDB $ selectList ([] :: [Filter UserInformation]) []
    
    courseraUser <- fmap Aeson.toJSON $ runDB $ selectList ([] :: [Filter CourseraUser]) []
    userConfirmation <- fmap Aeson.toJSON $ runDB $ selectList ([] :: [Filter UserConfirmation]) []
    contest <- fmap Aeson.toJSON $ runDB $ selectList ([] :: [Filter Contest]) []
    courseraContest <- fmap Aeson.toJSON $ runDB $ selectList ([] :: [Filter CourseraContest]) []
    post <- fmap Aeson.toJSON $ runDB $ selectList ([] :: [Filter Post]) []
    postDependency <- fmap Aeson.toJSON $ runDB $ selectList ([] :: [Filter PostDependency]) []
    judge <- fmap Aeson.toJSON $ runDB $ selectList ([] :: [Filter Judge]) []
    judgeConflict <- fmap Aeson.toJSON $ runDB $ selectList ([] :: [Filter JudgeConflict]) []
    buildJudgement <- fmap Aeson.toJSON $ runDB $ selectList ([] :: [Filter BuildJudgement]) []
    breakJudgement <- fmap Aeson.toJSON $ runDB $ selectList ([] :: [Filter BreakJudgement]) []
    fixJudgement <- fmap Aeson.toJSON $ runDB $ selectList ([] :: [Filter FixJudgement]) []
    breakDispute <- fmap Aeson.toJSON $ runDB $ selectList ([] :: [Filter BreakDispute]) []
    team <- fmap Aeson.toJSON $ runDB $ selectList ([] :: [Filter Team]) []
    teamContest <- fmap Aeson.toJSON $ runDB $ selectList ([] :: [Filter TeamContest]) []
    teamMember <- fmap Aeson.toJSON $ runDB $ selectList ([] :: [Filter TeamMember]) []
    teamInvite <- fmap Aeson.toJSON $ runDB $ selectList ([] :: [Filter TeamInvite]) []
    passwordResetInvite <- fmap Aeson.toJSON $ runDB $ selectList ([] :: [Filter PasswordResetInvite]) []
    contestCoreTest <- fmap Aeson.toJSON $ runDB $ selectList ([] :: [Filter ContestCoreTest]) []
    contestOptionalTest <- fmap Aeson.toJSON $ runDB $ selectList ([] :: [Filter ContestOptionalTest]) []
    contestPerformanceTest <- fmap Aeson.toJSON $ runDB $ selectList ([] :: [Filter ContestPerformanceTest]) []
    teamBreakScore <- fmap Aeson.toJSON $ runDB $ selectList ([] :: [Filter TeamBreakScore]) []
    teamBuildScore <- fmap Aeson.toJSON $ runDB $ selectList ([] :: [Filter TeamBuildScore]) []
    oracleSubmission <- fmap Aeson.toJSON $ runDB $ selectList ([] :: [Filter OracleSubmission]) []
    buildSubmission <- fmap Aeson.toJSON $ runDB $ selectList ([] :: [Filter BuildSubmission]) []
    breakOracleSubmission <- fmap Aeson.toJSON $ runDB $ selectList ([] :: [Filter BreakOracleSubmission]) []
    breakSubmission <- fmap Aeson.toJSON $ runDB $ selectList ([] :: [Filter BreakSubmission]) []
    fixSubmission <- fmap Aeson.toJSON $ runDB $ selectList ([] :: [Filter FixSubmission]) []
    fixSubmissionBugs <- fmap Aeson.toJSON $ runDB $ selectList ([] :: [Filter FixSubmissionBugs]) []
    buildCoreResult <- fmap Aeson.toJSON $ runDB $ selectList ([] :: [Filter BuildCoreResult]) []
    buildPerformanceResult <- fmap Aeson.toJSON $ runDB $ selectList ([] :: [Filter BuildPerformanceResult]) []
    buildOptionalResult <- fmap Aeson.toJSON $ runDB $ selectList ([] :: [Filter BuildOptionalResult]) []
    configuration <- fmap Aeson.toJSON $ runDB $ selectList ([] :: [Filter Configuration]) []
    cacheExpiration <- fmap Aeson.toJSON $ runDB $ selectList ([] :: [Filter CacheExpiration]) []
    cacheBuildersCode <- fmap Aeson.toJSON $ runDB $ selectList ([] :: [Filter CacheBuildersCode]) []
    storedFile <- fmap Aeson.toJSON $ runDB $ selectList ([] :: [Filter StoredFile]) []
    error <- fmap Aeson.toJSON $ runDB $ selectList ([] :: [Filter Error]) []
    rateLimitLog <- fmap Aeson.toJSON $ runDB $ selectList ([] :: [Filter RateLimitLog]) []
    scorePending <- fmap Aeson.toJSON $ runDB $ selectList ([] :: [Filter ScorePending]) []

    lift $ BSLC.putStrLn $ Aeson.encode [
        user
      , userInformation
      , courseraUser
      , userConfirmation
      , contest
      , courseraContest
      , post
      , postDependency
      , judge
      , judgeConflict
      , buildJudgement
      , breakJudgement
      , fixJudgement
      , breakDispute
      , team
      , teamContest
      , teamMember
      , teamInvite
      , passwordResetInvite
      , contestCoreTest
      , contestPerformanceTest
      , contestOptionalTest
      , contestPerformanceTest
      , teamBreakScore
      , teamBuildScore
      , oracleSubmission
      , buildSubmission
      , breakOracleSubmission 
      , breakSubmission
      , fixSubmission
      , fixSubmissionBugs
      , buildCoreResult
      , buildPerformanceResult
      , buildOptionalResult
      , configuration
      , cacheExpiration
      , cacheBuildersCode
      , storedFile
      , error
      , rateLimitLog
      , scorePending
      ]

    where
        anonymizeUser (Entity uId u) = 
          let u' = u {
                  userIdent = hashText (userIdent u)
                , userPassword = "redacted"
                , userEmail = hashText (userEmail u)
                }
          in
          Entity uId u'

        anonymizeUserInformation e@(Entity k ui) =
            if userInformationAgreeToParticipate ui then
                e
            else
                -- Default empty.
                Entity k $ UserInformation
                    (userInformationUser ui)
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                    ""
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                    False
                    Nothing
                    Nothing
                    Nothing
                    False
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                    Nothing


    --     selectToJson :: Proxy a
    --     selectToJson = toJSON $ selectList [] []
    
dump _ = boolFail "error: incorrect number of arguments"

hashText = Text.decodeUtf8 . hex . dropEnd 2 . hash . Text.encodeUtf8

    where dropEnd i s = BS.take (BS.length s - i) s

