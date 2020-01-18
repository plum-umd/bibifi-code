module Submissions where

import qualified Database.Esqueleto as E

import BuildSubmissions (isActiveBreak)
import Import
import PostDependencyType

displayFixSubmissionsTable details submissions = do
    let rows = map row submissions
    [whamlet|
        <table class="table table-hover">
            <thead>
                <tr>
                    ^{teamHeader}
                    <th>
                        Fix commit
                    <th>
                        Timestamp
                    <th>
                        Status
                    <th>
                        Result
            <tbody>
                ^{mconcat rows}
    |]
    clickableDiv

    where
        teamHeader = if details then
                [whamlet'|
                    <th>
                        Team
                |]
            else
                mempty

        row (Entity sId s) = do
            let status = prettyFixStatus $ fixSubmissionStatus s
            let result = prettyFixResult $ fixSubmissionResult s
            time <- displayTime $ fixSubmissionTimestamp s
            let team = if details then
                    [shamlet|
                        <td>
                            #{keyToInt $ fixSubmissionTeam s}
                    |]
                  else
                    mempty
            [whamlet|
              <tr .clickable href="@{ParticipationFixSubmissionR (fixSubmissionTeam s) sId}">
                  ^{team}
                  <td>
                      #{fixSubmissionCommitHash s}
                  <td>
                      #{time}
                  <td>
                      #{status}
                  <td>
                      #{result}
            |]
              
-- displayBuildSubmissionsTable :: a -> b -> LMonadT l (WidgetT site IO) ()
displayBuildSubmissionsTable details submissions = do
    let rows = mconcat $ map row submissions
    [whamlet|
        <table class="table table-hover">
            <thead>
                <tr>
                    #{teamHeader}
                    <th>
                        Submission hash
                    <th>
                        Timestamp
                    <th>
                        Status
            <tbody>
                ^{rows}
    |] :: LWidget
    clickableDiv

    where
        teamHeader = if details then
                [shamlet|
                    <th>
                        Team
                |]
            else
                mempty

        row (Entity sId s) = do
            let status = prettyBuildStatus $ buildSubmissionStatus s
            time <- displayTime $ buildSubmissionTimestamp s
            -- TODO: Show team name? XXX
            let team = if details then
                    [shamlet|
                        <td>
                            #{keyToInt $ buildSubmissionTeam s}
                    |]
                  else
                    mempty
            [whamlet|
                <tr class="clickable" href="@{ParticipationBuildSubmissionR (buildSubmissionTeam s) sId}">
                    #{team}
                    <td>
                        #{buildSubmissionCommitHash s}
                    <td>
                        #{time}
                    <td>
                        #{status}
            |] :: LWidget

data BreakSubmissionViewer = 
      BreakSubmissionAdmin
    | BreakSubmissionAttacker
    | BreakSubmissionVictim

displayBreakSubmissionsTable contest viewer submissions = do
    rows <- mapM (row viewer) submissions
    [whamlet|
        <table class="table table-hover">
            <thead>
                <tr>
                    ^{header viewer}
            <tbody>
                ^{mconcat rows}
    |]
    clickableDiv

    where
        -- row :: BreakSubmissionViewer -> (Entity BreakSubmission, Text) -> LWidget
        row BreakSubmissionVictim (Entity sId s, attackerM) = do
            let status = prettyBreakStatusVictim $ breakSubmissionStatus s
            let result = prettyBreakValidVictim $ breakSubmissionValid s
            let bType = maybe dash prettyBreakType $ breakSubmissionBreakType s
            let (Just targetTeamId) = breakSubmissionTargetTeam s
            let attacker = maybe dash toHtml attackerM
            fixStatus <- prettyFixStatus sId
            time <- liftIO $ displayTime $ breakSubmissionTimestamp s
            now <- getCurrentTime
            let name = 
                  if now > contestBreakEnd contest then 
                      toHtml $ breakSubmissionName s 
                  else
                      dash
            return [whamlet'|
              <tr .clickable href="@{ParticipationBreakSubmissionR targetTeamId sId}">
                  <td>
                      #{name} (#{keyToInt sId})
                  <td>
                      #{time}
                  <td>
                      #{attacker} (#{keyToInt $ breakSubmissionTeam s})
                  <td>
                      #{status}
                  <td>
                      #{result}
                  <td>
                      #{bType}
                  <td>
                      #{fixStatus}
            |]

        row BreakSubmissionAttacker (Entity sId s, targetM) = do
            let status = prettyBreakStatus $ breakSubmissionStatus s
            let result = prettyBreakValid $ breakSubmissionValid s
            let target = maybe dash toHtml targetM
            time <- liftIO $ displayTime $ breakSubmissionTimestamp s
            return [whamlet'|
                <tr .clickable href="@{ParticipationBreakSubmissionR (breakSubmissionTeam s) sId}">
                    <td>
                        #{breakSubmissionName s}
                    <td>
                        #{time}
                    <td>
                        #{target}
                    <td>
                        #{status}
                    <td>
                        #{result}
            |]

        row BreakSubmissionAdmin (Entity sId s, targetM) = do
            let status = prettyBreakStatus $ breakSubmissionStatus s
            let result = prettyBreakValid $ breakSubmissionValid s
            let target = maybe dash toHtml targetM
            time <- liftIO $ displayTime $ breakSubmissionTimestamp s
            return [whamlet'|
                <tr .clickable href="@{ParticipationBreakSubmissionR (breakSubmissionTeam s) sId}">
                    <td>
                        #{keyToInt $ breakSubmissionTeam s}
                    <td>
                        #{breakSubmissionName s}
                    <td>
                        #{time}
                    <td>
                        #{target}
                    <td>
                        #{status}
                    <td>
                        #{result}
            |]

        header BreakSubmissionVictim = [whamlet'|
                <th>
                    Test name
                <th>
                    Timestamp
                <th>
                    Attacking Team
                <th>
                    Status
                <th>
                    Result
                <th>
                    Type
                <th>
                    Fix Status
            |]

        header BreakSubmissionAttacker = [whamlet'|
                <th>
                    Test name
                <th>
                    Timestamp
                <th>
                    Target Team
                <th>
                    Status
                <th>
                    Result
            |]

        header BreakSubmissionAdmin = [whamlet'|
                <th>
                    Team
                <th>
                    Test name
                <th>
                    Timestamp
                <th>
                    Target Team
                <th>
                    Status
                <th>
                    Result
            |]
        
        prettyFixStatus bsId = handlerToWidget $ do
            disputeM <- runDB $ getBy $ UniqueBreakDispute bsId
            case disputeM of
                Just _ ->
                    return [shamlet|
                        <span>
                            Disputed
                    |]
                Nothing -> do
                    active <- runDB $ isActiveBreak bsId
                    if active then
                        return [shamlet|
                            <span .text-danger>
                                Active
                        |]
                    else
                        return [shamlet|
                            <span .text-success>
                                Fixed
                        |]

                    -- -- Check if a non pending/rejected fix exists.
                    -- fixs <- runDB $ E.select $ E.from $ \(E.InnerJoin f bf) -> do
                    --     E.on (E.just (f E.^. FixSubmissionId) E.==. bf E.^. BreakFixSubmissionFix)
                    --     E.where_ (bf E.^. BreakFixSubmissionBreak E.==. E.val bsId E.&&.
                    --         (f E.^. FixSubmissionStatus E.==. E.val FixBuilt E.||. f E.^. FixSubmissionStatus E.==. E.val FixJudging E.||. f E.^. FixSubmissionStatus E.==. E.val FixJudged))
                    --     return bf
                    -- case fixs of
                    --     [_a] ->
                    --         return [shamlet|
                    --             <span>
                    --                 Submitted
                    --         |]
                    --     _ ->
                    --         return dash
