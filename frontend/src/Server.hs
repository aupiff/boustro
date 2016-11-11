{-# LANGUAGE TemplateHaskell #-}

module Server
     ( contextText
     ) where

import           Data.FileEmbed

contextText :: String
contextText = $(embedStringFile "texts/p_war.txt")
