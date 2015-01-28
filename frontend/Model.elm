module Model where

import Html (Html)
import Dict (Dict)
import UI (ViewDimensions)

type alias AppState = { fullText     : String
                      , viewDims     : ViewDimensions
                      , currentPage  : Html
                      , priorPages   : List Html
                      , futurePages  : List Html
                      }
