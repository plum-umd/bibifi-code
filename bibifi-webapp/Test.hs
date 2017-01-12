-- Module that contains some shared forms for creating/editing tests.

module Test where

import qualified Data.Aeson as Aeson
import qualified Data.Text.Encoding as Text

import Import

data FormData = FormData {
      formDataName :: Text
    , formDataTest :: Textarea
    }

testForm def = renderBootstrap3 BootstrapBasicForm $ FormData
    <$> areq textField nameSettings (fmap fst def)
    <*> areq (check validateJson textareaField) testSettings (fmap (Textarea . snd) def)

    where
        nameSettings = withPlaceholder "Test Name" $ bfs ("Test Name" :: Text)
        testSettings = withPlaceholder "Test Script (JSON)" $ bfs ("Test Script (JSON)" :: Text)

        validateJson t = case Aeson.decodeStrict $ Text.encodeUtf8 $ unTextarea t of
            Nothing ->
                Left ("Not valid JSON." :: Text)
            Just (_ :: Aeson.Value)  ->
                Right t
