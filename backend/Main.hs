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
import           Servant.HTML.Lucid
import           Data.Monoid ((<>))

type API = Get '[HTML] WelcomePage
      :<|> "app"   :> Raw
      :<|> "texts" :> Raw

data WelcomePage = WelcomePage

instance ToHtml WelcomePage where
  toHtml p = html_ $ do

    head_ $ title_ "Boustro"

    body_ $ do
        h1_ "Boustrophedon Syndicate"
        p_ "hello" <> p_ "sup"
        a_ [href_ "/app"] "boustrophedon reading application"


api :: Proxy API
api = Proxy

server :: Server API
server = return WelcomePage
    :<|> serveDirectory "static/texts"
    :<|> serveDirectory "static"

app :: Application
app = serve api server

main :: IO ()
main = N.run 80 app
