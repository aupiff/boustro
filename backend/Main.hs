{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad.Trans
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.ByteString (ByteString)
import           Control.Lens
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Char8 as BS
import           Data.Time.Clock
import qualified Database.PostgreSQL.Simple as P
import           Snap
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.PostgresqlSimple
import           Snap.Snaplet.Heist
import           Snap.Snaplet.PostgresqlSimple
import           Snap.Snaplet.Session
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
import           Heist
import           Text.XmlHtml hiding (render)


------------------------------------------------------------------------------
data App = App
    { _sess :: Snaplet SessionManager
    , _db :: Snaplet Postgres
    , _auth :: Snaplet (AuthManager App)
    }

makeLenses ''App

instance HasPostgres (Handler b App) where
    getPostgresState = with db get
    setLocalPostgresState s = local (set (db . snapletValue) s)

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/", serveFile "frontend/boustro.html")
         , ("elm.js", serveFile "frontend/elm.js")
         , ("style.css", serveFile "frontend/style.css")
         , ("texts", serveDirectory "texts")
         , ("listtexts", showTextsHandler)
         , ("hyphenation", serveDirectory "hyphenation")
         , ("user", showUsersHandler)
         , ("user/:uname", addUserHandler)
         ]

data TextPart = TextPart
  { title :: T.Text
  , path  :: T.Text
  } deriving (Show, Eq)

instance FromRow TextPart where
     fromRow = TextPart <$> field <*> field

-- TODO add type annotations to all of these
showTextsHandler = do
    results <- query_ "select * from texts"
    writeBS . BS.pack $ show (results :: [TextPart])

showUsersHandler = do
    results <- query_ "select * from snap_auth_user"
    writeBS . BS.pack $ show (results :: [AuthUser])

addUserHandler = do
    mname <- getParam "uname"
    let name = maybe "guest" T.decodeUtf8 mname
    u <- with auth $ createUser name ""
    writeBS . BS.pack $ show u

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    s <- nestSnaplet "" sess $
         initCookieSessionManager "site_key.txt" "_cookie" Nothing
    d <- nestSnaplet "db" db pgsInit
    a <- nestSnaplet "auth" auth $ initPostgresAuth sess d
    addRoutes routes
    return $ App s d a


main :: IO ()
main = serveSnaplet defaultConfig app
