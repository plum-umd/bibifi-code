module Submissions where

import Import
import PostDependencyType

displayBuildSubmissionsTable submissions = do
    let rows = mconcat $ map row submissions
    [whamlet|
        <table class="table table-hover">
            <thead>
                <tr>
                    <th>
                        Submission hash
                    <th>
                        Timestamp
                    <th>
                        Status
            <tbody>
                ^{rows}
    |]
    clickableDiv

    where
        row (Entity sId s) = do
            let status = prettyBuildStatus $ buildSubmissionStatus s
            time <- lift $ displayTime $ buildSubmissionTimestamp s
            [whamlet'|
                <tr class="clickable" href="@{ParticipationBuildSubmissionR (buildSubmissionTeam s) sId}">
                    <td>
                        #{buildSubmissionCommitHash s}
                    <td>
                        #{time}
                    <td>
                        #{status}
            |]
