{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Directory
import Database.PostgreSQL.Simple
import Control.Monad
import Control.Applicative

cInfo = ConnectInfo { connectHost = "127.0.0.1"
                    , connectPort = 5432
                    , connectUser = "postgres"
                    , connectPassword = "pass"
                    , connectDatabase = "testdb"
                    }

main :: IO ()
main = do contents <- System.Directory.getDirectoryContents "texts"
          let texts = filter ((/= '.') . head) contents
          conn <- connect cInfo
          execute conn "DELETE FROM texts" ()
          mapM_ (\title -> execute conn "INSERT INTO texts VALUES (?, ?)" (title, title)) texts
