module Handler.Contest.Participation where

import Import
import qualified Team

getContestParticipationR :: Text -> Handler Html
getContestParticipationR cUrl = runLHandler $ do
    uId  <- requireAuthId
    contest' <- runDB $ getBy $ UniqueContest cUrl
    case contest' of 
        Nothing ->
            notFound
        Just (Entity cId contest) ->
            do
            res' <- runDB $ Team.getTeamContest uId cId
            case res' of 
                [tcId] ->
                    redirect $ ParticipationInformationR tcId
                _ -> do
                    setMessage [shamlet|
                        <div class="container">
                            <div class="alert alert-warning">
                                Your are not participating in that contest.
                    |]
                    redirect $ ContestSpecificSignupR $ contestUrl contest
    
