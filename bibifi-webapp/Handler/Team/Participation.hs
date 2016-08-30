module Handler.Team.Participation where

import Import
import qualified Team

getTeamParticipationR :: TeamId -> Handler Html
getTeamParticipationR tId = runLHandler $ Team.layout Team.ContestParticipation tId $ \_uId _team -> do
    contestParticipations <- handlerToWidget $ runDB [lsql| select TeamContest.id, Contest.title from TeamContest inner join Contest on TeamContest.contest == Contest.id where TeamContest.team == #{tId} order by Contest.buildStart desc |]
    -- contestParticipations <- handlerToWidget $ runDB $ E.select $ E.from $ \(tc `E.InnerJoin` c) -> do
    --     E.on ( tc E.^. TeamContestContest E.==. c E.^. ContestId)
    --     E.where_ ( tc E.^. TeamContestTeam E.==. E.val tId)
    --     E.orderBy [E.desc (c E.^. ContestBuildStart)]
    --     return ( tc E.^. TeamContestId, c E.^. ContestTitle)
    case contestParticipations of
        [] ->
            [whamlet|
                This team has not signed up to participate in any contests.
            |]
        _ -> do
            let display ( tcId, cName) = do
                [whamlet'|
                    <a href="@{ParticipationInformationR tcId}" class="list-group-item">
                        #{cName}
                |]
            let cs = mconcat $ map display contestParticipations
            toWidget listGroupStyle
            [whamlet|
                <ul class="list-group vertical-margin">
                    ^{cs}
            |]

