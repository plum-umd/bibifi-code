module Handler.Participation where

import Import
import qualified Team

getParticipationR :: Handler Html
getParticipationR = runLHandler $ do
    uId  <- requireAuthId
    -- Get default contest.
    res <- retrieveContest Nothing
    case res of
        Nothing ->
            notFound
        Just (Entity cId c) -> do
            -- Get tc for active contest.
            res' <- runDB $ Team.getTeamContest uId cId
            case res' of 
                [tcId] ->
                    redirect $ ParticipationInformationR tcId
                _ -> do
                    setMessage [shamlet|
                        <div class="container">
                            <div class="alert alert-warning">
                                Your are not participating in the current contest. Sign your team up!
                    |]
                    redirect $ ContestSpecificSignupR $ contestUrl c

