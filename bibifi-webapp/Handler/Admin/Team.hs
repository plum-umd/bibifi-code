module Handler.Admin.Team where

import qualified Admin
import Import

getAdminTeamR :: TeamId -> Handler Html
getAdminTeamR tId = runLHandler $ Admin.layout Admin.Teams $ do
    team <- handlerToWidget $ runDB $ get404 tId
    Admin.setTitle $ teamName team
    [whamlet|
        <a href="@{AdminTeamsR}" type="button" class="btn btn-primary">
            Back
        <h2>
            Team #{teamName team}
    |]
    toWidget listGroupStyle
    contestsWidget
    membersWidget team

    where
        membersWidget team = do
            [whamlet|
                <h3>
                    Manage Members
            |]
            let leaderId = teamLeader team
            leader <- handlerToWidget $ runDB $ get404 leaderId
            members <- handlerToWidget $ runDB [lsql|
                    select User.id, User.ident from User
                    inner join TeamMember on TeamMember.user == User.id
                    where TeamMember.team == #{tId}
                |]
            let ms = mconcat $ map (memberWidget False) members
            [whamlet|
                <ul .list-group>
                    ^{memberWidget True (leaderId, userIdent leader)}
                    ^{ms}
            |]

        memberWidget leader (memberId, memberIdent) = 
            let leaderW = 
                  if leader then
                    [whamlet'|
                        <span .pull-right .label .label-primary>
                            Leader
                    |]
                  else
                    mempty
            in
            [whamlet'|
                <a href="@{AdminTeamMemberR tId memberId}" .list-group-item>
                    #{memberIdent}
                    ^{leaderW}
            |]

        contestsWidget = do
            contests <- handlerToWidget $ runDB [lsql|
                    select Contest.url, Contest.title from Contest
                    inner join TeamContest on Contest.id == TeamContest.contest
                    where TeamContest.team == #{tId}
                |]
            [whamlet|
                <h3>
                    Registered for the Following Contests
            |]
            if null contests then
                [whamlet|
                    <p>
                        This team is not registered for any contests.
                |]
            else do
                let cs = mconcat $ map contestWidget contests
                [whamlet|
                    <ul .list-group>
                        ^{cs}
                |]

        contestWidget (cUrl, cName) = [whamlet'|
                <a href="@{AdminContestR cUrl}" .list-group-item>
                    #{cName}
            |]

