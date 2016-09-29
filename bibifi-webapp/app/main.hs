import Prelude              (IO, return, ($))
import Yesod.Default.Config (fromArgs, fromArgsSettings, ConfigSettings(..), configSettings)
import Yesod.Default.Main   (defaultMain)
import Settings             (parseExtra)
import Application          (makeApplication)

main :: IO ()
main = defaultMain (fromArgs' parseExtra) makeApplication

    where
        fromArgs' getExtra = fromArgsSettings $ \env -> return (configSettings env)
            { csParseExtra = getExtra
            , csFile = \_ -> return "../config/settings.yml"
            }
