{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Directory
import Database.PostgreSQL.Simple
import Control.Monad
import Control.Applicative

main :: IO ()
main = do contents <- System.Directory.getDirectoryContents "texts"
          let texts = filter ((/= '.') . head) contents
          conn <- connect defaultConnectInfo { connectDatabase = "testdb" }
          execute conn "DELETE FROM texts" ()
          mapM_ (\title -> execute conn "INSERT INTO texts VALUES (?, ?)" (title, title)) texts
