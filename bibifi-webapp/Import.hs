{-# LANGUAGE FlexibleInstances, InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Import
    ( module Import
    , getCurrentTime
    ) where

import           Prelude              as Import hiding (head, init, last,
                                                 readFile, tail, writeFile)
import           LYesod               as Import

import           Control.Applicative  as Import (pure, (<$>), (<*>), (<*))
import           Control.Monad        as Import
import           Data.Either          as Import (isLeft)
import           Data.Text            as Import (Text)
import           Data.Time            as Import (UTCTime, addUTCTime, NominalDiffTime)

import           Foundation           as Import
import           Foundation.App       as Import
import           Model                as Import
import           Settings             as Import
import           Settings.Development as Import
import           Settings.StaticFiles as Import

import           Config               as Import
import           Contest              as Import
import           Common               as Import

import           LMonad               as Import
import           LMonad.Label.DisjunctionCategory as Import
import           LMonad.Yesod         as Import
import           TCB                  as Import

import           Database.LPersist    as Import
import           Database.LEsqueleto  as Import

import     Database.Persist.RateLimit as Import

import          Yesod.Form.Bootstrap3 as Import

import          RateLimit             as Import

#if __GLASGOW_HASKELL__ >= 704
import           Data.Monoid          as Import
                                                 (Monoid (mappend, mempty, mconcat),
                                                 (<>))
#else
import           Data.Monoid          as Import
                                                 (Monoid (mappend, mempty, mconcat))

infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
#endif

import qualified Data.Time.Clock      as Clock

getCurrentTime = liftIO Clock.getCurrentTime

whenJust (Just _) m = m
whenJust Nothing _ = return ()

data FormAndHandler = forall a . FormAndHandler (Form a) (FormResult a -> Widget -> Enctype -> LWidget)

runMultipleFormsPost :: [FormAndHandler] -> LWidget
runMultipleFormsPost [] = return ()
runMultipleFormsPost ((FormAndHandler form handler):t) = do
    ((res, widget), enctype) <- handlerToWidget $ runFormPost form
    case res of
        FormMissing ->
            runMultipleFormsPost t
        _ ->
            handler res widget enctype


