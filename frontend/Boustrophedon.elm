import List as L
import String
import Html (..)
import Graphics.Element (..)
import Dict
import Signal as S
import Signal ((<~), (~), Signal)
import Window
import UI (..)
import Server
import Model (..)
import Utils
import Typography
import Maybe as M

stringToState : String -> ViewDimensions -> AppState
stringToState str viewDims =
    let linesPerPage = viewDims.textHeight // lineHeight
        wordArray = Typography.strToWordArray str
        wordIndex = 0
        (page, wc) = Typography.typesetPage viewDims.textWidth
                                            linesPerPage
                                            wordIndex
                                            wordArray
    in  { fullText         = wordArray
        , viewDims         = viewDims
        , currentPage      = page
        , wordIndex        = wordIndex
        , pageWordCount    = wc
        }

nextState : InputData -> AppState -> AppState
nextState userInput pState =
    case userInput of
        SetText str     -> stringToState str pState.viewDims
        ViewUpdate dims ->
            let linesPerPage = dims.textHeight // lineHeight
                (page, wc) = Typography.typesetPage dims.textWidth
                                                    linesPerPage
                                                    pState.wordIndex
                                                    pState.fullText
            in { fullText         = pState.fullText
               , viewDims         = dims
               , currentPage      = page
               , wordIndex        = pState.wordIndex
               , pageWordCount    = wc
               }
        Swipe Next  ->
            let linesPerPage = pState.viewDims.textHeight // lineHeight
                wordIndex = pState.wordIndex + pState.pageWordCount
                (page, wc) = Typography.typesetPage pState.viewDims.textWidth
                                                    linesPerPage
                                                    wordIndex
                                                    pState.fullText
            in { fullText         = pState.fullText
               , viewDims         = pState.viewDims
               , currentPage      = page
               , wordIndex        = wordIndex
               , pageWordCount    = wc
               }
        Swipe NoSwipe -> pState
        otherwise       -> pState
        --Swipe Prev  ->
        --    if | L.isEmpty pState.priorPages -> pState
        --       | otherwise ->
        --             { fullText    = pState.fullText
        --             , viewDims    = pState.viewDims
        --             , currentPage = L.head pState.priorPages
        --             , priorPages  = L.tail pState.priorPages
        --             , futurePages = pState.currentPage :: pState.futurePages
        --             }

emptyState = stringToState Server.default_text <| viewHelper (600, 300)

appState : Signal AppState
appState = S.foldp nextState emptyState userInput

userInput : Signal UserInput
userInput = S.mergeMany [ S.map SetText Server.textContent
                        , S.map Swipe swipe
                        , S.map ViewUpdate currentViewDimensions
                        ]

scene : AppState -> Element
scene appState =
    let viewDims = appState.viewDims
        renderTextView = toElement viewDims.textWidth viewDims.textHeight
        fullContainer = container viewDims.fullContainerWidth
                                  viewDims.fullContainerHeight
                                  middle
    in  fullContainer <| renderTextView appState.currentPage

main : Signal Element
main = scene <~ appState
