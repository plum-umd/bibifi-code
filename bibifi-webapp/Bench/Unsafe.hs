module Bench.Unsafe where

import Import hiding (runDB, deleteWhere, insertKey)
import Database.Esqueleto hiding ((!=.))
import Database.Persist (deleteWhere, insertKey)
import Database.Persist.Sql
import qualified Data.Text as T
import qualified Yesod as Y
import qualified Yesod.Auth as Y

getBenchInsertR :: Int -> Handler Html
getBenchInsertR c = do
    -- Delete all users except admin.
    Y.runDB $ deleteWhere [UserId !=. toKey 1]

    now <- liftIO getCurrentTime

    -- Insert c - 1 users.
    mapM_ (\c -> 
        let k = toKey c in
        let u = User (T.pack $ show c) "pass" "" (T.pack $ show c ++ "@email.com") now False Nothing Nothing in
        Y.runDB $ insertKey k u
      ) [2..c]

    return [shamlet|<div>done|]

    where toKey = toSqlKey . fromInteger . toInteger

getBenchUnsafeR :: Handler Html
getBenchUnsafeR = do
    (Entity uId user) <- Y.requireAuth
    when (not $ userAdmin user) $ permissionDenied ""

    res <- Y.runDB $ select $ from $ \u -> do
        return (u ^. UserIdent, u ^. UserEmail)
    rs <- mapM displayUser res

    -- defaultLayout $ do
    return $ 
        case res of
            [] -> 
                [shamlet|
                    <p>
                        There are no users.
                |]
            _ -> do
                [shamlet|
                    <div>
                        #{mconcat rs}
                |]
    where
        -- displayUser :: (Labeled (DCLabel Principal) Text, Labeled (DCLabel Principal) Text) -> LHandler Html
        displayUser (Value ident, Value email) = do
            return [shamlet|
                <div>#{ident} - #{email}
            |]
