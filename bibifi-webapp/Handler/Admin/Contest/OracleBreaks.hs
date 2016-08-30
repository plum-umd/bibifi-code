module Handler.Admin.Contest.OracleBreaks where

import Import
import qualified Admin
import qualified Database.Esqueleto as E

-- List current oracle breaks. 
getAdminContestOracleBreaksR :: Text -> Handler Html
getAdminContestOracleBreaksR url = runLHandler $ do
    res <- retrieveContest $ Just url
    Admin.layout Admin.Contests $ 
        case res of 
            Nothing ->
                Admin.contestNotFound
            Just (Entity cId c) -> do
                Admin.setTitle $ contestTitle c
                [whamlet|
                    <a href="@{AdminContestR url}" type="button" class="btn btn-primary">
                        Back
                    <h2>
                        Oracle Breaks
                        <small>
                            #{contestTitle c}
                    <a href="@{AdminContestOracleBreaksCreateR url}" type="button" class="btn btn-primary pull-right">
                        New oracle break
                    <div class="clearfix">
                |]
                breaks <- handlerToWidget $ runDB $ E.select $ E.from $ \(E.InnerJoin (E.InnerJoin t tc) b) -> do
                    E.on (tc E.^. TeamContestId E.==. b E.^. BreakOracleSubmissionTeam)
                    E.on (t E.^. TeamId E.==. tc E.^. TeamContestTeam)
                    E.where_ (tc E.^. TeamContestContest E.==. E.val cId)
                    return (t E.^. TeamName, b)
                case breaks of
                    [] ->
                        [whamlet|
                            <p>
                                There are no oracle breaks.
                        |]
                    _ -> do
                        let rows = mconcat $ map (\(E.Value team, Entity bosId bos) -> do
                                time <- lift $ displayTime $ breakOracleSubmissionTimestamp bos
                                let valid = if breakOracleSubmissionValid bos then
                                        "Yes" :: Text
                                      else
                                        "No"
                                [whamlet'|
                                    <tr class="clickable" href="@{AdminContestOracleBreaksEditR bosId}">
                                        <td>
                                            #{team} (#{keyToInt (breakOracleSubmissionTeam bos)})
                                        <td>
                                            #{time}
                                        <td>
                                            #{valid}
                                        <td>
                                            #{breakOracleSubmissionDescription bos}
                                |]
                              ) breaks
                        [whamlet|
                            <table class="table table-hover">
                                <thead>
                                    <tr>
                                        <th>
                                            Team
                                        <th>
                                            Timestamp
                                        <th>
                                            Valid
                                        <th>
                                            Description
                                <tbody>
                                    ^{rows}
                        |]
                        clickableDiv

