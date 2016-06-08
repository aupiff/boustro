{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Aeson
import           Data.Time.Calendar
import           GHC.Generics
import           Network.Wai
import qualified Network.Wai.Handler.Warp as N
import           Servant
import           Control.Monad.IO.Class
import           Text.Hyphenation.Hyphenator
import           Lucid
import           Lucid.Base
import           Servant.HTML.Lucid
import           Data.Monoid ((<>))

type API = Get '[HTML] WelcomePage
      :<|> "app"   :> Raw

data WelcomePage = WelcomePage

instance ToHtml WelcomePage where
  toHtml p = html_ $ do

    head_ $ do
      title_ "Boustro"
      meta_ [charset_ "UTF-8"]
      meta_ [name_ "apple-mobile-web-app-capable", content_ "yes"]
      meta_ [name_ "viewport", content_ "width=device-width, height=device-height, initial-scale=1.0, maximum-scale=1.0, minimum-scale=1.0, user-scalable=no"]


    body_ [style_ "text-align: center"] $ do
        object_ [id_ "svg1", makeAttribute "data" "/app/boustro-logo.svg", type_ "image/svg+xml"] $ return ()
        p_ "Efficient Software" <> p_ "contact: riblankman@gmail.com"
        a_ [href_ "/app", style_ "display:block"] "Boustrophedon Reading Application"


api :: Proxy API
api = Proxy

server :: Server API
server = return WelcomePage
    :<|> serveDirectory "static"

app :: Application
app = serve api server

main :: IO ()
main = N.run 80 app
