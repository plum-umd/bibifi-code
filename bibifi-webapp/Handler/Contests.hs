{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Contests (getContestsR) where

import Import

-- render :: Entity Contest -> Handler _--(HtmlUrl url)
-- TODO: render :: Entity Contest -> Handler Html
render c = 
    let Entity _ contest = c in 
    let url = contestUrl contest in
    lLift $ do
        start <- displayTime $ contestBuildStart contest
        end <- displayTime $ contestBreakEnd contest
        return $ [hamlet|$newline never
            <div>
                <div>
                    <a href=@{SpecificAnnouncementsR url}>
                       #{contestTitle contest}
                <div>
                    #{start} to #{end}
        |]

getContestsR :: Handler Html
getContestsR = runLHandler $ do
    contests <- runDB $ selectList [] [Asc ContestBuildStart]
    ham <- case contests of
        [] ->
            return $ [hamlet|Sorry, no contests found.|]
        cs -> do
            rendered <- mapM render cs
            return $ mconcat rendered
    defaultLayout $ do
        setTitle "Past Contests"
        toWidget [hamlet|$newline never
            <div class="row">
                <div class="col-md-12">
                    <div class="page-header">
                        <h1>
                            Past Contests 
                            <small>
                                BIBIFI Contest
            <div class="row">
                <div class="col-md-12">
                    ^{ham}
        |]

