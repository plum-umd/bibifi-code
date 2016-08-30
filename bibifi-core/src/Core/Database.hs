{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, ConstraintKinds #-}

module Core.Database where

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
-- import Database.Persist.Class
import Database.Persist.Sql (SqlBackend)
-- import Yesod.Core
-- import Yesod.Persist.Core

class Monad m => GeneralPersist site m | m -> site where
    type GeneralPersistBackend site
    runDB' :: ReaderT (GeneralPersistBackend site) m a -> m a
    -- runDB' :: (PersistEntityBackend a ~ GeneralPersistBackend site) => ReaderT (GeneralPersistBackend site) m a -> m a

class (GeneralPersistBackend site ~ SqlBackend, MonadIO m, GeneralPersist site m) => GeneralPersistSql site m

-- instance YesodPersist site => GeneralPersist site (HandlerT site IO) where
--     type GeneralPersistBackend site = YesodPersistBackend site
--     runDB' = runDB
