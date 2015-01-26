module Model where

import Html (Html)

type alias AppState = { fullText     : String
                      , currentPage  : Html
                      , priorPages   : List Html
                      , futurePages  : List Html
                      }