{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Participation.BuildersCode where

import Data.Ord

import Import
import qualified Cache
import qualified Participation

getParticipationBuildersCodeR :: TeamContestId -> Handler Html
getParticipationBuildersCodeR tcId = runLHandler $ 
    Participation.layout Participation.BuilderCode tcId $ \uId tc contest team -> do
        -- Check if break/fix-it round has started.
        now <- getCurrentTime
        if now < contestBreakFixStart contest then
            notFound
        else
            do
            sortParam <- lookupGetParam "sort"
            let (direction, vulnHeader) = 
                  let vulnHeader = [whamlet'|
                        <th .clickable href="@{ParticipationBuildersCodeR tcId}">
                            Vulnerabilities
                    |]
                  in
                  let vulnHeaderSorted = [whamlet'|
                        <th>
                            Vulnerabilities
                            <span class="glyphicon glyphicon-chevron-up">
                    |]
                  in
                  case sortParam of
                    Just "score" ->
                      --(cacheBuildersCodeBuilderScore :: Ord o => CacheBuildersCode -> o, vulnHeader)
                      (comparing (Down . cacheBuildersCodeBuilderScore), vulnHeader)
                      --(Desc CacheBuildersCodeBuilderScore, vulnHeader)
                    Just "bug" ->
                      (comparing cacheBuildersCodeBugsFound, vulnHeader)
                      --(Asc CacheBuildersCodeBugsFound, vulnHeader)
                    _ ->
                      (comparing cacheBuildersCodeVulnerabilitiesFound, vulnHeaderSorted)
                      --(Asc CacheBuildersCodeVulnerabilitiesFound, vulnHeaderSorted)
            let bugHeader = case sortParam of
                  Just "bug" -> [whamlet'|
                          <th>
                              Bugs
                              <span class="glyphicon glyphicon-chevron-up">
                      |]
                  _ -> [whamlet'|
                          <th .clickable href="@{ParticipationBuildersCodeR tcId}?sort=bug">
                              Bugs
                      |]
            let scoreHeader = case sortParam of
                  Just "score" -> [whamlet'|
                        <th>
                            Builder Score
                            <span class="glyphicon glyphicon-chevron-down">
                    |]
                  _ -> [whamlet'|
                        <th .clickable href="@{ParticipationBuildersCodeR tcId}?sort=score">
                            Builder Score
                    |]
            cachedResults <- handlerToWidget $ Cache.buildersCode (teamContestContest tc) direction tcId
            let rows = case cachedResults of
                  [] ->
                    [whamlet'|
                        <tr>
                            <td colspan="6">
                                There are no builder submissions.
                    |]
                  _ ->
                    let disp (Entity _ (CacheBuildersCode name tId lang _ bs bugs vulns)) = 
                          let teamId = keyToInt tId in
                          [whamlet'|
                              <tr>
                                  <td>
                                    #{name} (<a href="/static/doc/code/#{contestUrl contest}/#{teamId}.zip">code</a>)
                                  <td>
                                    #{teamId}
                                  <td>
                                    #{lang}
                                  <td>
                                    #{bs}
                                  <td>
                                    #{bugs}
                                  <td>
                                    #{vulns}
                          |]
                    in
                    mconcat $ map disp cachedResults
            [whamlet|
                <table class="table">
                    <thead>
                        <tr>
                            <th>
                                Team
                            <th>
                                Team Id
                            <th>
                                Languages
                            ^{scoreHeader}
                            ^{bugHeader}
                            ^{vulnHeader}
                    <tbody>
                        ^{rows}
                <p .text-muted>
                    Note: This table updates periodically.
            |] :: LWidget
            -- Styling
            toWidget [lucius|
                .clickable:hover {
                    background-color: #f5f5f5;
                }
            |]
            clickableDiv
