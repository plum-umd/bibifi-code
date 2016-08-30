module Handler.Admin.Contest.Judgements where

import Control.Monad
import qualified Database.Esqueleto as E

import qualified Admin
import BuildSubmissions
import Import
import PostDependencyType

getAdminContestJudgementsR :: Text -> Handler Html
getAdminContestJudgementsR url = runLHandler $ do
    res <- retrieveContest $ Just url
    Admin.layout Admin.Contests $ do
        case res of 
            Nothing ->
                Admin.contestNotFound
            Just (Entity cId c) -> do
                Admin.setTitle $ contestTitle c
                buildRows <- do
                    -- Retrieve latest build submissions for each (non-prof) team. 
                    -- TODO: Fix this XXX
                    --buildSubmissions <- handlerToWidget $ runDB $ E.select $ E.from $ \(E.RightOuterJoin j (E.InnerJoin (E.LeftOuterJoin bs bs') tc)) -> do
                    buildSubmissions <- handlerToWidget $ runDB $ do
                        s <- getLatestBuildSubmissions cId $ \tc bs ->
                            return (bs, tc E.^. TeamContestLanguages)
                        -- s <- E.select $ E.from $ \(E.InnerJoin tc (E.LeftOuterJoin bs bs')) -> do
                        -- --E.on ( j E.?. BuildJudgementSubmission E.==. E.just (bs E.^. BuildSubmissionId))
                        --     E.on ( E.just ( bs E.^. BuildSubmissionTimestamp) E.<. bs' E.?. BuildSubmissionTimestamp E.&&. E.just ( bs E.^. BuildSubmissionTeam) E.==. bs' E.?. BuildSubmissionTeam)
                        --     E.on ( tc E.^. TeamContestId E.==. bs E.^. BuildSubmissionTeam)
                        --     --E.orderBy [E.Asc (j E.^. BuildJudgementRuling)]
                        --     E.where_ ( tc E.^. TeamContestContest E.==. E.val cId E.&&. E.isNothing (bs' E.?. BuildSubmissionTeam) E.&&. tc E.^. TeamContestProfessional E.==. E.val False)
                        --     return (bs, tc E.^.TeamContestLanguages)
                        -- Filter out ineligible builders and get judgements.
                        foldM (\acc (bs',l) ->
                                let (Entity bsId _) = bs' in
                                do
                                passesRequired <- buildSubmissionPassesRequiredTests cId bsId
                                if passesRequired then
                                    do
                                    jM <- getBy $ UniqueBuildJudgement bsId
                                    return $ ( bs', E.unValue l, jM):acc
                                else
                                    return acc
                            ) [] s
                    return $ mconcat $ map (\((Entity bsId bs), lang, judgementM) ->
                            let ( judge, judgement) = case judgementM of
                                  Nothing -> 
                                    ( dash, dash)
                                  Just (Entity _ j) ->
                                    let status = rulingToHtml $ buildJudgementRuling j in
                                    ( [shamlet|#{keyToInt (buildJudgementJudge j)}|], status)
                            in
                            [whamlet'|
                                <tr .clickable href="@{AdminContestJudgementsAssignBuildR url bsId}">
                                    <td>
                                        #{buildSubmissionCommitHash bs}
                                    <td>
                                        #{lang}
                                    <td>
                                        #{judge}
                                    <td>
                                        #{judgement}
                            |]
                        ) buildSubmissions
                breakRows <- handlerToWidget $ runDB $ do
                    bss' <- [lsql|
                            select BreakSubmission.*, TeamContest.languages from TeamContest
                            inner join BreakSubmission on TeamContest.id == BreakSubmission.targetTeam
                            left outer join BreakDispute on BreakDispute.break == BreakSubmission.id
                            where TeamContest.contest == #{cId}
                                and (BreakSubmission.status == #{BreakJudging} or BreakSubmission.status == #{BreakJudged} or BreakDispute.id is not null)
                        |]
                    -- getLatestBreakSubmissions cId $ \tc bs -> do
                    --     E.where_ ( bs E.^. BreakSubmissionStatus E.==. E.val BreakJudging E.||. 
                    --         bs E.^. BreakSubmissionStatus E.==. E.val BreakJudged)
                    --     return (bs, tc E.^. TeamContestLanguages)
                    ws <- mapM (\( (Entity bsId bs), lang) -> 
                            do
                            jM <- getBy $ UniqueBreakJudgement bsId
                            let ( judge, judgement) = case jM of
                                  Nothing ->
                                    ( dash, dash)
                                  Just (Entity _ j) ->
                                    let status = rulingToHtml $ breakJudgementRuling j in
                                    ( [shamlet|#{keyToInt (breakJudgementJudge j)}|], status)
                            return $ [whamlet'|
                                <tr .clickable href="@{AdminContestJudgementsAssignBreakR url bsId}">
                                    <td>
                                        #{breakSubmissionName bs}
                                    <td>
                                        #{lang}
                                    <td>
                                        #{judge}
                                    <td>
                                        #{judgement}
                            |]
                        ) bss'
                    return $ mconcat $ ws 
                fixRows <- handlerToWidget $ runDB $ do 
                    fs' <- [lsql| 
                            select FixSubmission.*, TeamContest.languages from TeamContest
                            inner join FixSubmission on TeamContest.id == FixSubmission.team
                            where TeamContest.contest == #{cId}
                                and (FixSubmission.status == #{FixJudging} or FixSubmission.status == #{FixJudged})
                        |]
                    -- getLatestFixSubmissions cId $ \tc fs -> do
                    --     E.where_ ( fs E.^. FixSubmissionStatus E.==. E.val FixJudging E.||. 
                    --         fs E.^. FixSubmissionStatus E.==. E.val FixJudged)
                    --     return (fs, tc E.^. TeamContestLanguages)
                    ws <- mapM (\( (Entity fsId fs), lang) -> do
                            jM <- getBy $ UniqueFixJudgement fsId
                            let ( judge, judgement) = case jM of
                                  Nothing -> 
                                    ( dash, dash)
                                  Just ( Entity _ j) ->
                                        let status = rulingToHtml $ fixJudgementRuling j in
                                        ( [shamlet|#{keyToInt (fixJudgementJudge j)}|], status)
                            return $ [whamlet'|
                                <tr .clickable href="@{AdminContestJudgementsAssignFixR url fsId}">
                                    <td>
                                        #{fixSubmissionName fs}
                                    <td>
                                        #{lang}
                                    <td>
                                        #{judge}
                                    <td>
                                        #{judgement}
                            |]
                        ) fs'
                    return $ mconcat $ ws 
                [whamlet|
                    <a href="@{AdminContestR url}" type="button" class="btn btn-primary">
                        Back
                    <h2>
                        Judgements
                        <small>
                            #{contestTitle c}
                    <table class="table table-hover">
                        <thead>
                            <tr>
                                <th>
                                    Submission
                                <th>
                                    Language
                                <th>
                                    Judge
                                <th>
                                    Judgement
                        <tbody>
                            ^{fixRows}
                            ^{breakRows}
                            ^{buildRows}
                |]
                clickableDiv
    where
        rulingToHtml ruling = case ruling of
            Nothing ->
              [shamlet|
                  <span .text-danger>
                      Incomplete
              |]
            Just True ->
              [shamlet|
                  <span>
                      Complete (Pass)
              |]
            Just False ->
              [shamlet|
                  <span .text-warning>
                      Complete (Fail)
              |]
