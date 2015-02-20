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
routes = [ ("/", method GET $ serveFile "frontend/boustro.html")
         , ("elm.js", method GET $ serveFile "frontend/elm.js")
         , ("style.css", method GET $ serveFile "frontend/style.css")
         , ("texts", method GET $ serveDirectory "texts")
         , ("text", method GET showTextsHandler)
         , ("text", method POST addTextHandler)
         , ("hyphenation", method GET $ serveDirectory "hyphenation")
         , ("user", method GET showUsersHandler)
         , ("user/:uname", method POST addUserHandler)
         ]

data TextPart = TextPart
  { title :: T.Text
  , path  :: T.Text
  } deriving (Show, Eq)

instance FromRow TextPart where
     fromRow = TextPart <$> field <*> field

showTextsHandler :: Handler App App ()
showTextsHandler = do
    results <- query_ "select * from texts"
    writeBS . BS.pack $ show (results :: [TextPart])

addTextHandler :: Handler App App ()
addTextHandler = do
  title <- getPostParam "title"
  path <- getPostParam "path"
  newTextPart <- execute "INSERT INTO texts VALUES (?, ?)" (title, path)
  writeBS . BS.pack $ show newTextPart

showUsersHandler :: Handler App App ()
showUsersHandler = do
    results <- query_ "select * from snap_auth_user"
    writeBS . BS.pack $ show (results :: [AuthUser])

addUserHandler :: Handler App App ()
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
