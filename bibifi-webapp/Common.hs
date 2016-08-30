module Common (
      randomString
    , contestTimeZone
    , displayTime
    , displayError
    , listGroupStyle
    , clickableDiv
    , addAttribute
    , bootstrapify
    , renderBootstrap3'
    , sidebarCss
    , dash
    , keyToInt
    , emailLink
    ) where

import Core
import Control.Monad.Random
import Css
import Prelude as P
import Data.Char
import Data.Text as T
import Data.Time
import Database.Persist.Types
import System.Locale hiding (defaultTimeLocale)
import Text.Hamlet
import Yesod

-- Generate random strings.
-- https://www.b7j0c.org/blog/random_strings_in_haskell.html
randomString :: Int -> IO Text
randomString l = do
    values <- evalRandIO (sequence (P.replicate l (getRandomR (65,90))))
    return $ T.pack $ P.map chr values

contestTimeZone :: IO TimeZone
contestTimeZone = getCurrentTimeZone

-- Displays and formats time to a string.
displayTime :: UTCTime -> IO String
displayTime t' = do
    tz <- contestTimeZone
    let t = utcToZonedTime tz t'
    return $ formatTime defaultTimeLocale "%Y.%m.%d %H:%M %Z" t
    --return $ formatTime defaultTimeLocale "%Y.%m.%d %H:%M %Z" t'

-- Formats form error messages.
displayError :: Text -> HtmlUrl url
displayError s = [hamlet|$newline never
    <div class="text-danger">
        #{s}
|]

-- listGroupStyle :: Css
listGroupStyle = 
    [lucius|
        .vertical-margin {
            margin-top: 10px;
            margin-bottom: 10px;
        }
    |]

clickableDiv = 
    do
    toWidget [lucius|
        .clickable {
            cursor: pointer;
        } 
    |]
    toWidget [julius|
        jQuery(document).ready(function($) {
          $(".clickable").click(function() {
            window.document.location = $(this).attr("href");
          });
        });
    |]

addAttribute :: FieldSettings a -> Text -> Text -> FieldSettings a
addAttribute fs' k v = 
    let oldAttrs = fsAttrs fs' in
    let attrs = ( k, v):oldAttrs in
    fs' {fsAttrs = attrs}

bootstrapify :: FieldSettings a -> FieldSettings a
bootstrapify fs = addAttribute fs "class" "form-control"
    
-- TODO: deprecate this and migrate to Yesod.Form.Bootstrap3
renderBootstrap3' :: Monad m => FormRender m a
renderBootstrap3' aform fragment = do
    (res, views') <- aFormToForm aform
    let views = views' []
        has (Just _) = True
        has Nothing  = False
    let widget = [whamlet|
                $newline never
                \#{fragment}
                $forall view <- views
                    <div .form-group>
                        <label .col-sm-2 .control-label for=#{fvId view}>#{fvLabel view}
                        <div .col-sm-10>
                            ^{fvInput view}
                            $maybe tt <- fvTooltip view
                                <span .help-block>#{tt}
                            $maybe err <- fvErrors view
                                <span .help-block>#{err}
                |]

-- TODO: rename to horizontal; fix errors; use has, required, etc

--                     <div .control-group .clearfix :fvRequired view:.required :not $ fvRequired view:.optional :has $ fvErrors view:.error>
--                         <label .control-label for=#{fvId view}>#{fvLabel view}
--                         <div .controls .input>
--                             ^{fvInput view}
--                             $maybe tt <- fvTooltip view
--                                 <span .help-block>#{tt}
--                             $maybe err <- fvErrors view
--                                 <span .help-block>#{err}
    return (res, widget)

sidebarCss sidebarStyle = 
    toWidget [lucius|
        ##{sidebarStyle} {
            text-shadow: 0 1px 0 #fff;

            > .nav {
                margin: 40px 0px;
                padding-top: 10px;
                padding-bottom: 10px;
                background-color: #ffe7e6;
                border-radius: 5px;

                > .active {
                    > a {
                        font-weight: bold;
                    }
                }

                li {
                    a {
                        display: block;
                        color: #770600;
                        padding-top: 7px;
                        padding-bottom: 7px;
                    }

                    a:hover, a:focus {
                        background-color: #f7d7d6;
                    }
                }

                .nav {
                    li {
                        a {
                            padding-top: 2px;
                            padding-bottom: 2px;
                            padding-left: 30px;
                            font-size: 90%;
                        }
                    }

                    > .active {
                        > a {
                            font-weight: bold;
                        }
                    }
                }
            }
        }

        @media (min-width: #{screen_lg_min}) {
            ##{sidebarStyle}.affix {
                top: 0px;
                width: 263px;
            }
        }

        @media (min-width: #{screen_md_min}) and (max-width: #{screen_lg_min}) {
            ##{sidebarStyle}.affix {
                top: 0px;
                width: 213px;
            }
        }

        @media (max-width: #{screen_md_min}) {
            ##{sidebarStyle}.affix {
                position: static;
            }
        }
    |]

dash :: Html
dash = [shamlet|&#8212;|]

-- emailLink :: Widget
emailLink = [shamlet|<script>document.write( atob('PGEgaHJlZj0ibWFpbHRvOmluZm9AYnVpbGRpdGJyZWFraXQub3JnIj5pbmZvQGJ1aWxkaXRicmVha2l0Lm9yZzwvYT4='));</script>|]
