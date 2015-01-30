module Model where

import Array (Array)

type alias AppState = { fullText  : Array String
                      , wordIndex : Int
                      }
