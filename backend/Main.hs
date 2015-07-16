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

type API = "app" :> Raw :<|> "texts" :> Raw

api :: Proxy API
api = Proxy

server :: Server API
server = serveDirectory "static" :<|> serveDirectory "static/texts"

app :: Application
app = serve api server

main :: IO ()
main = N.run 80 app
