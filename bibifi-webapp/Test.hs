-- Module that contains some shared forms for creating/editing tests.

module Test where

import qualified Data.Aeson as Aeson
import qualified Data.Text.Encoding as Text

import Forms
import Import

data PerformanceFormData = PerformanceFormData {
      performanceFormDataName :: Text
    , performanceFormDataTest :: Textarea
    , performanceFormDataRequired :: Bool
    }

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

performanceTestForm def = renderBootstrap3 BootstrapBasicForm $ PerformanceFormData
    <$> areq textField nameSettings (fmap (\(x,_,_) -> x) def)
    <*> areq (check validateJson textareaField) testSettings (fmap (\(_,x,_) -> Textarea x) def)
    <*> areq (checkBoxField' ("Required test" :: Text)) requiredSettings (Just $ maybe True (\(_,_,x) -> x) def)

    where
        nameSettings = withPlaceholder "Test Name" $ bfs ("Test Name" :: Text)
        testSettings = withPlaceholder "Test Script (JSON)" $ bfs ("Test Script (JSON)" :: Text)
        requiredSettings = "Required"

        validateJson t = case Aeson.decodeStrict $ Text.encodeUtf8 $ unTextarea t of
            Nothing ->
                Left ("Not valid JSON." :: Text)
            Just (_ :: Aeson.Value)  ->
                Right t
