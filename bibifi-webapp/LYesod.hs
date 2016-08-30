module LYesod (
        module Export
      , requireAuth'
      , requireAuthId'
      , maybeAuth'
      , maybeAuthId'
      , widgetToPageContent'
      , whamlet'
--       , tempRunDB
--      , setMessage
--      , redirectUltDest
      , generateFormPost
      , runFormPost
      ) where

import           Control.Monad.Reader as Export (ReaderT)
import           Database.Persist.Sql as Export (SqlBackend)
import           LMonad
import           Prelude
import           Yesod                as Export hiding (
                                            Route (..)
                                          , runDB
                                          , insertUnique
                                          , getBy
                                          , deleteBy
                                          , updateGet
                                          , update
                                          , repsert
                                          , insert_
                                          , insertMany
                                          , insertKey
                                          , insert
                                          , get
                                          , delete
                                          , updateWhere
                                          , selectFirst
                                          , deleteWhere
                                          , getJust
                                          , selectList
                                          , selectKeysList
                                          , replace
                                          , count
                                          , handlerToWidget
                                          , widgetToPageContent
                                          , whamlet
                                          , Value(..)
                                          -- Optional (for convenience):
                                          , Yesod (defaultLayout)
                                          , generateFormPost
                                          , runFormPost
--                                          , setMessage
--                                          , redirectUltDest
                                          )
import qualified Yesod as Yesod
import           Yesod.Auth           as Export hiding (
                                          -- Optional (for convenience):
                                            requireAuth
                                          , requireAuthId
                                          , maybeAuth
                                          , maybeAuthId
                                          )
import qualified Yesod.Auth as Yesod

requireAuth' = Yesod.requireAuth
requireAuthId' = Yesod.requireAuthId
maybeAuth' = Yesod.maybeAuth
maybeAuthId' = Yesod.maybeAuthId
widgetToPageContent' = Yesod.widgetToPageContent
whamlet' = Yesod.whamlet

-- Some convenience functions to make conversion easier.

generateFormPost = lLift . Yesod.generateFormPost
runFormPost = lLift . Yesod.runFormPost
-- tempRunDB = lLift . Yesod.runDB
-- setMessage = lLift . Yesod.setMessage
-- redirectUltDest = lLift . Yesod.redirectUltDest
