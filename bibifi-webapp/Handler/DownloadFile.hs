module Handler.DownloadFile where

import Control.Monad
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import Database.Persist.Sql (rawExecute)

import Import

-- Reference: https://www.fpcomplete.com/school/to-infinity-and-beyond/competition-winners/part-5

getDownloadFileR :: StoredFileId -> Handler TypedContent
getDownloadFileR fileId = runLHandler $ do
    file <- runDB $ do
        rawExecute "set bytea_output = 'escape';" []
        get fileId
    case file of 
        Nothing ->
            notFound
        Just (StoredFile mOwner name contentType content) -> do
            -- Return not found if not the owner.
            case mOwner of
                Nothing -> 
                    return ()
                Just owner -> do
                    (Entity userId user) <- requireAuth
                    when (userId /= owner && not (userAdmin user))
                        notFound
            -- Deliver the file.
            addHeader "Content-Disposition" $ Text.concat [ "attachment; filename=\"", name, "\""]
            sendResponse ( Text.encodeUtf8 contentType, toContent content)
