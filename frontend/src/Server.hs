{-# LANGUAGE TemplateHaskell #-}

module Server
     ( texts
     ) where

import           Data.FileEmbed

texts :: [String]
texts = [ $(embedStringFile "texts/rabbit.txt")
        , $(embedStringFile "texts/frog.txt")
        , $(embedStringFile "texts/reading.txt")
        , $(embedStringFile "texts/crowd.txt")
        , $(embedStringFile "texts/eveline.txt")
        , "Mirrored text may be difficult to read at first, but it becomes natural with practice. Learning to read like this will eliminate much of the wasted eye-movement required when we read only from left-to-right."
        ]
