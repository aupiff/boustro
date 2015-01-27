module Model where

import Html (Html)

type alias AppState = { fullText     : String
                      , charWidths   : List (Int, Char)
                      , currentPage  : Html
                      , priorPages   : List Html
                      , futurePages  : List Html
                      }
