module Handler.Admin.Contests (getAdminContestsR) where

import Import
import qualified Admin

getAdminContestsR :: Handler Html
getAdminContestsR = runLHandler $ Admin.layout Admin.Contests $ do
    Admin.setTitle "Contests"
    res <- handlerToWidget $ runDB $ selectList [] [Desc ContestBuildStart]
    [whamlet|
        <a href="@{TodoR}" type="button" class="btn btn-primary pull-right">
            Create new contest
        <div class="clearfix">
    |]
    case res of
        [] ->
            [whamlet|
                There are no contests.
            |]
        contests -> do
            isDefault <- handlerToWidget $ isDefaultContest
            let display c =
                  let def = isDefault c in
                  let (Entity _ c') = c in
                  let def' = 
                        if def then
                            [whamlet'|<span class="pull-right label label-primary">Default</span>|]
                        else
                            mempty
                  in
                  [whamlet'|
                      <a href="@{AdminContestR (contestUrl c')}" class="list-group-item">
                          #{contestTitle c'}
                          ^{def'}
                                              
                  |]
            let cs = mconcat $ map display contests
            toWidget listGroupStyle
            [whamlet|
                <ul class="list-group vertical-margin">
                    ^{cs}
            |]

