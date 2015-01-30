module Model where

import Array (Array)

type alias ModelState = { fullText  : Array String
                        , wordIndex : Int
                        }
