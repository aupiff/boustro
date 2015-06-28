{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DeriveGeneric #-}

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
import           Data.Aeson.Types (ToJSON)
import qualified Data.ByteString.Char8 as BS
import           Data.Time.Clock
import           GHC.Generics
import           Snap
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Extras.JSON (writeJSON)
import           Snap.Util.FileServe
import           Heist
import           Text.XmlHtml hiding (render)


------------------------------------------------------------------------------
data App = App { _sess :: Snaplet SessionManager }

makeLenses ''App

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/", method GET $ serveFile "frontend/boustro.html")
         , ("elm.js", method GET $ serveFile "frontend/elm.js")
         , ("style.css", method GET $ serveFile "frontend/style.css")
         , ("texts", method GET $ serveDirectory "texts")
         ]

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    s <- nestSnaplet "" sess $
         initCookieSessionManager "site_key.txt" "_cookie" Nothing
    addRoutes routes
    return $ App s


main :: IO ()
main = serveSnaplet defaultConfig app
