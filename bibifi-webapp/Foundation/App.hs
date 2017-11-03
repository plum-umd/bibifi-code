module Foundation.App where

import Database.LPersist as LP
import Data.Text (Text)
import qualified Database.Persist
import LMonad
import LMonad.Label.DisjunctionCategory
import LMonad.Yesod
import Model
import Network.HTTP.Conduit (Manager)
import Prelude
import qualified Settings
import Settings (Extra (..))
import Text.Shakespeare.I18N
import qualified Yesod
import Yesod.Auth
import Yesod.Core
import Yesod.Core.Types (Logger)
import Yesod.Default.Config
import Yesod.Static

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { settings :: AppConfig DefaultEnv Extra
    , getStatic :: Static -- ^ Settings for static file serving.
    , connPool :: Database.Persist.PersistConfigPool Settings.PersistConf -- ^ Database connection pool.
    , httpManager :: Manager
    , persistConfig :: Settings.PersistConf
    , appLogger :: Logger
    }

-- Set up i18n messages. See the message folder.
mkMessage "App" "messages" "en"

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/handler
--
-- This function does three things:
--
-- * Creates the route datatype AppRoute. Every valid URL in your
--   application can be represented as a value of this type.
-- * Creates the associated type:
--       type instance Route App = AppRoute
-- * Creates the value resourcesApp which contains information on the
--   resources declared below. This is used in Handler.hs by the call to
--   mkYesodDispatch
--
-- What this function does *not* do is create a YesodSite instance for
-- App. Creating that instance requires all of the handler functions
-- for our application to be in scope. However, the handler functions
-- usually require access to the AppRoute datatype. Therefore, we
-- split these actions into two functions and place them in separate files.
mkYesodData "App" $(parseRoutesFile "../config/routes")

--
-- Noninterference stuff. 
--

type LHandler = LMonadT (DCLabel Principal) Handler

instance LMonad Handler where
    lFail = permissionDenied "Sorry, you do not have permission to view this page."
    lAllowLift = return True
    
type LWidget = LMonadT (DCLabel Principal) (Yesod.WidgetT App IO) ()

instance LMonad (Yesod.WidgetT App IO) where
    lFail = Yesod.handlerToWidget lFail
    lAllowLift = Yesod.handlerToWidget lAllowLift

instance YesodLPersist App where
    runDB = lDefaultRunDB persistConfig connPool

runLHandler :: LHandler a -> Handler a
runLHandler = runLMonad

--
-- End noninterference stuff. 
--

