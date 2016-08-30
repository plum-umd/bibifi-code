module Handler.Admin.Contest.JudgeEmails where

import Control.Monad
import qualified Data.Text as Text

import qualified Admin
import Import
import qualified Team

getAdminContestJudgeEmailsR :: Text -> Handler Html
getAdminContestJudgeEmailsR url = runLHandler $ do
    res <- retrieveContest $ Just url
    Admin.layout Admin.Contests $ do
        case res of 
            Nothing ->
                Admin.contestNotFound
            Just (Entity cId c) -> do
                Admin.setTitle $ contestTitle c
                judgeEmails <- do
                    emails <- handlerToWidget $ runDB $ [lsql|
                            select User.email from User inner join Judge
                            on User.id == Judge.judge
                            where Judge.contest == #{cId}
                        |]
                    -- $ E.select $ E.from $ \(u `E.InnerJoin` j) -> do
                    --     E.on (u E.^. UserId E.==. j E.^. JudgeJudge)
                    --     E.where_ (j E.^. JudgeContest E.==. E.val cId)
                    --     return (u E.^. UserEmail)
                    return $ case emails of
                        [] ->
                            "No judges"
                        _ ->
                            Text.intercalate "," emails
                profEmails <- handlerToWidget $ runDB $ do
                    profTeams <- selectList [TeamContestContest ==. cId,TeamContestProfessional ==. True] []
                    profs <- foldM (\acc (Entity _ tc) -> do
                            members <- Team.getTeamMembers (teamContestTeam tc)
                            return $ members ++ acc
                        ) [] profTeams
                    emails <- mapM (\uId -> do
                            uM <- get uId
                            case uM of
                                Nothing ->
                                    error "getAdminContestJudgeEmailsR: should never happen"
                                Just u ->
                                    return $ userEmail u
                        ) profs
                    return $ case emails of 
                        [] ->
                            "No professional team members"
                        _ ->
                            Text.intercalate "," emails
                [whamlet|
                    <a href="@{AdminContestR url}" type="button" class="btn btn-primary">
                        Back
                    <h2>
                        Judges's Information
                        <small>
                            #{contestTitle c}
                    <h3>
                        Judges's emails
                    <pre .pre-scrollable>
                        #{judgeEmails}
                    <h3>
                        Professional breakers's emails
                    <pre .pre-scrollable>
                        #{profEmails}
                |]
