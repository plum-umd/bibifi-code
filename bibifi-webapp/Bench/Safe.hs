module Bench.Safe where

import Import

-- import Data.Proxy

getBenchSafeR :: Handler Html
getBenchSafeR = runLHandler $ do
    -- liftIO $ putStrLn "****** here *********"
    -- cl <- getCurrentLabel
    -- cc <- getClearance
    -- liftIO $ putStrLn $ show cl
    -- liftIO $ putStrLn $ show cc
    raiseUserLabel
    -- liftIO $ putStrLn "****** here2 *********"
    -- cl <- getCurrentLabel
    -- cc <- getClearance
    -- liftIO $ putStrLn $ show cl
    -- liftIO $ putStrLn $ show cc
    -- liftIO $ putStrLn $ show $ tableLabel (Proxy :: Proxy User)
    res <- runDB [lsql| pselect ident, email from User|]
    -- liftIO $ putStrLn "****** here3 *********"
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
        displayUser :: (Labeled (DCLabel Principal) Text, Labeled (DCLabel Principal) Text) -> LHandler Html
        displayUser (pIdent, pEmail) = do
            ident <- unlabel pIdent
            email <- unlabel pEmail
            return [shamlet|
                <div>#{ident} - #{email}
            |]
