module Coursera where

import qualified Data.List as List
import Database.LPersist
-- import Foundation
import LYesod
import Model
import Prelude
import Yesod.Auth.OAuth2.Coursera

data CourseraStatus = 
        CourseraStatusEnrolled
      | CourseraStatusNotCoursera
      | CourseraStatusNotEnrolled
      | CourseraStatusError
    deriving (Eq, Show)

-- | Get enrollment status for coursera contests and users. 
checkCourseraContest :: ContestId -> UserId -> LHandler CourseraStatus
checkCourseraContest contestId userId = do
    courseraM <- runDB $ getBy $ UniqueCourseraContest contestId
    case courseraM of
        Nothing ->
            return CourseraStatusNotCoursera
        Just (Entity _ (CourseraContest _ courseId sessionId)) -> do
            -- Get user token.
            cUserM <- runDB $ getBy $ UniqueCourseraUser userId
            case cUserM of
                Nothing ->
                    return CourseraStatusNotEnrolled
                Just (Entity _ (CourseraUser _ _ token)) -> do
                    -- Request courses from Coursera. 
                    -- TODO: Cache this request?
                    manager <- fmap httpManager getYesod
                    enrollmentsE <- liftIO $ oauth2CourseraEnrollments manager token
                    return $ case enrollmentsE of
                        Left _ ->
                            CourseraStatusError
                        Right (Enrollments es) ->
                            -- Check if courseId is in courses. 
                            if List.any (\(Enrollment cId sId) -> cId == courseId && sId == sessionId) es then
                                CourseraStatusEnrolled
                            else
                                CourseraStatusNotEnrolled

