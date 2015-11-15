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

type API = "app"         :> Raw
      :<|> "texts"       :> Raw
      :<|> "/"           :> Get '[HTML] WelcomePage

data WelcomePage = WelcomePage { message :: String }

page = WelcomePage "hi"

instance ToHtml WelcomePage where
  toHtml person = toHtml $ message page

api :: Proxy API
api = Proxy

server :: Server API
server = serveDirectory "static"
    :<|> serveDirectory "static/texts"
    :<|> return page

app :: Application
app = serve api server

main :: IO ()
main = N.run 80 app
