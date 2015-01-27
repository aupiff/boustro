module Model where

import Html (Html)
import Dict (Dict)

type alias AppState = { fullText     : String
                      , charWidths   : Dict Char Int
                      , currentPage  : Html
                      , priorPages   : List Html
                      , futurePages  : List Html
                      }
