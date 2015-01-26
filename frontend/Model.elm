module Model where

import Html (Html)

type alias AppState = { fullText     : String
                      , uniqChars    : List Char
                      , currentPage  : Html
                      , priorPages   : List Html
                      , futurePages  : List Html
                      }
