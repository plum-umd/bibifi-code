module Handler.Admin.Contest (getAdminContestR) where

import Import
import qualified Admin

getAdminContestR :: Text -> Handler Html
getAdminContestR url = runLHandler $ do
    res <- retrieveContest $ Just url
    Admin.layout Admin.Contests $ 
        case res of 
            Nothing ->
                Admin.contestNotFound
            Just (Entity _ c) -> do
                Admin.setTitle $ contestTitle c
                [whamlet|
                    <a href="@{AdminContestsR}" type="button" class="btn btn-primary">
                        Back
                    <h2>
                        #{contestTitle c}
                    <h2>
                        Edit
                    <p>
                        <a href="@{AdminContestEditR url}">
                            Edit contest name, date, etc.
                    <h3>
                        Announcements
                    <p>
                        <a href="@{TodoR}">
                            Create new announcements, edit them, delete them, etc. 
                    <h3>
                        Participants
                    <p>
                        <a href="@{TodoR}">
                            View participants, inspect their submissions, etc.
                    <p>
                        <a href="@{AdminContestParticipantEmailsR url}">
                            View participants' emails.
                    <h3>
                        Judges
                    <p>
                        <a href="@{AdminContestJudgeEmailsR url}">
                            View judges's information.
                    <p>
                        <a href="@{AdminContestJudgementsR url}">
                            View judgements.
                    <h3>
                        Default
                    <p>
                        <a href="@{MakeDefaultR url}">
                            Make this contest the default contest.
                    <h3>
                        Grading
                    <p>
                        <a href="@{AdminContestOracleBreaksR url}">
                            Award points for breaks in the oracle.
                |]
    
