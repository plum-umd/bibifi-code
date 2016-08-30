module Handler.Team.Information where

import qualified Data.Text as Text
import Import
import qualified Team

getTeamInformationR :: TeamId -> Handler Html
getTeamInformationR tId = runLHandler $ Team.layout Team.Information tId $ \_uId team -> do
        leaderW <- do
            mLeader <- handlerToWidget $ runDB $ get $ teamLeader team
            return $ case mLeader of
                Nothing ->
                    mempty
                Just leader ->
                    [whamlet'|
                        <label class="col-sm-2 control-label">
                            Leader
                        <div class="col-sm-10">
                            <p class="form-control-static">
                                #{userIdent leader}
                    |]
        membersH <- do
            members <- handlerToWidget $ runDB $ [lsql| select User.ident from TeamMember inner join User on TeamMember.user == User.id where TeamMember.team == #{tId}|]
            -- $ E.select $ E.from $ \(E.InnerJoin tm u) -> do
            --     E.on (tm E.^. TeamMemberUser E.==. u E.^. UserId)
            --     E.where_ (tm E.^. TeamMemberTeam E.==. E.val tId)
            --     return $ u E.^. UserIdent
            return $ case members of
                [] ->
                    "No other team members."
                _ -> 
                    Text.intercalate ", " members
        [whamlet|
            <form class="form-horizontal">
                <div class="form-group">
                    <label class="col-sm-2 control-label">
                        Team
                    <div class="col-sm-10">
                        <p class="form-control-static">
                            #{teamName team}
                    ^{leaderW}
                    <label class="col-sm-2 control-label">
                        Members
                    <div class="col-sm-10">
                        <p class="form-control-static">
                            #{membersH}
        |]

