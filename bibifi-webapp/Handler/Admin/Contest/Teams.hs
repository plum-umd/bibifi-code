module Handler.Admin.Contest.Teams where

import qualified Admin
import Import

getAdminContestTeamsR :: Text -> Handler Html
getAdminContestTeamsR url = runLHandler $ Admin.layoutContest url $ \(Entity contestId _) -> do
    Admin.setTitle "Participating teams"
    teams <- handlerToWidget $ runDB $ [lsql|
            select Team.* from Team
            inner join TeamContest on TeamContest.team == Team.id
            where TeamContest.contest == #{contestId}
        |]
    
    [whamlet|
        <a href="@{AdminContestR url}" type="button" .btn .btn-primary>
            Back
        <h2>
            Participating teams
    |]
    case teams of 
        [] ->
            [whamlet|
                <p>
                    There are no teams.
            |]
        _ -> do
            let ts = mconcat $ map display teams
            toWidget listGroupStyle
            [whamlet|
                <ul .list-group .vertical-margin>
                    ^{ts}
            |]

    where
        display (Entity tId t) = 
            [whamlet'|
                <a href="@{AdminTeamR tId}" .list-group-item>
                    #{teamName t}
            |]
