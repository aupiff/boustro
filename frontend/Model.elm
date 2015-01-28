module Model where

import Html (Html)
import Dict (Dict)

type alias AppState = { currentPage  : Html
                      , priorPages   : List Html
                      , futurePages  : List Html
                      }
