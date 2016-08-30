module Handler.Admin.Contest.ParticipantEmails where

import Control.Monad
import qualified Data.Text as Text
import qualified Database.Esqueleto as E

import qualified Admin
import BuildSubmissions
import Import
import qualified Team (getTeamMembers)

getAdminContestParticipantEmailsR :: Text -> Handler Html
getAdminContestParticipantEmailsR url = runLHandler $ do
    res <- retrieveContest $ Just url
    Admin.layout Admin.Contests $ do
        case res of 
            Nothing ->
                Admin.contestNotFound
            Just (Entity cId c) -> do
                Admin.setTitle $ contestTitle c
                ( builderEs, qualifiedEs) <- handlerToWidget $ runDB $ do
                    latestBuildSubmissions <- getLatestBuildSubmissions cId $ \tc bs ->
                        return (tc E.^. TeamContestTeam, bs E.^. BuildSubmissionId)
                    ( allEmails', passEmails') <- foldM (\(accAll,accPass) (E.Value tId, E.Value bsId) -> do
                        qualified <- buildSubmissionPassesRequiredTests cId bsId
                        members <- Team.getTeamMembers tId
                        foldM (\acc uId -> do
                            let (accAll,accPass) = acc
                            userM <- get uId
                            return $ case userM of
                                Nothing ->
                                    acc
                                Just u ->
                                    let email = (userEmail u) in
                                    let accPass' = if qualified then
                                            email:accPass
                                          else
                                            accPass
                                    in
                                    (email:accAll,accPass')
                          ) (accAll,accPass) members
                      ) ([],[]) latestBuildSubmissions
                    let allEmails = case allEmails' of
                          [] -> 
                              "No builder emails found."
                          _ -> 
                              Text.intercalate "," allEmails'
                    let passEmails = case passEmails' of
                          [] ->
                              "No qualified emails found."
                          _ ->
                              Text.intercalate "," passEmails'
                    return ( allEmails, passEmails)
                allRegisteredEs <- handlerToWidget $ runDB $ do 
                    registeredTeams <- E.select $ E.from $ \tc -> do
                        E.where_ (tc E.^. TeamContestContest E.==. E.val cId)
                        return $ tc E.^. TeamContestTeam
                    emails <- foldM (\acc (E.Value tId) -> do
                        members <- Team.getTeamMembers tId
                        foldM (\acc uId -> do
                            userM <- get uId
                            return $ maybe acc (\u -> userEmail u:acc) userM
                          ) acc members
                      ) [] registeredTeams
                    return $ case emails of
                        [] ->
                            "No registered emails found."
                        _ ->
                            Text.intercalate "," emails
                [whamlet|
                    <a href="@{AdminContestR url}" type="button" class="btn btn-primary">
                        Back
                    <h2>
                        Paricipant's Information
                        <small>
                            #{contestTitle c}
                    <h3>
                        Builder emails
                    <pre .pre-scrollable>
                        #{builderEs}
                    <h3>
                        Qualifying builder emails
                    <pre .pre-scrollable>
                        #{qualifiedEs}
                    <p .text-muted>
                        Note: Emails of teams that are passing all core/performance tests.
                    <h3>
                        All registered emails
                    <pre .pre-scrollable>
                        #{allRegisteredEs}
                |]
    
