{-# LANGUAGE TemplateHaskell #-}

module Server
     ( texts
     ) where

import           Data.FileEmbed

texts :: [String]
texts = [ $(embedStringFile "texts/rabbit.txt")
        , $(embedStringFile "texts/reading.txt")
        , $(embedStringFile "texts/eveline.txt")
        ]
