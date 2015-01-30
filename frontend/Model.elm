module Model where

import Html (Html)
import Dict (Dict)
import UI (ViewDimensions)
import Array (Array)

type alias AppState = { fullText         : Array String
                      , viewDims         : ViewDimensions
                      , currentPage      : Html
                      , wordIndex        : Int
                      , pageWordCount    : Int
                      }
