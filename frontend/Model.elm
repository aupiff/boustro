module Model where

import Html (Html)
import Dict (Dict)
import UI (ViewDimensions)
import Array (Array)

-- TODO I think this should be simplified to text, dims, wordIndex
type alias AppState = { fullText         : Array String
                      , viewDims         : ViewDimensions
                      , currentPage      : Html
                      , wordIndex        : Int
                      , pageWordCount    : Int
                      }
