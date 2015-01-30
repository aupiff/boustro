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
import Maybe as M
import Debug (log)
import Typography

stringToState : String -> ViewDimensions -> AppState
stringToState str viewDims =
    let wordArray = Typography.strToWordArray str
        wordIndex = 0
        (page, wc) = Typography.typesetPage viewDims.textWidth
                                            viewDims.linesPerPage
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
            let (page, wc) = Typography.typesetPage dims.textWidth
                                                    dims.linesPerPage
                                                    pState.wordIndex
                                                    pState.fullText
                a = log "word index" pState.wordIndex
            in { fullText         = pState.fullText
               , viewDims         = dims
               , currentPage      = page
               , wordIndex        = pState.wordIndex
               , pageWordCount    = wc
               }
        Swipe Next  ->
            let wordIndex = pState.wordIndex + pState.pageWordCount
                (page, wc) = Typography.typesetPage pState.viewDims.textWidth
                                                    pState.viewDims.linesPerPage
                                                    wordIndex
                                                    pState.fullText
                a = log "word index" wordIndex
            in { fullText         = pState.fullText
               , viewDims         = pState.viewDims
               , currentPage      = page
               , wordIndex        = wordIndex
               , pageWordCount    = wc
               }
        Swipe Prev  ->
            let (page, wc) = Typography.typesetPrevPage pState.viewDims.textWidth
                                                        pState.viewDims.linesPerPage
                                                        pState.wordIndex
                                                        pState.fullText
                wordIndex = pState.wordIndex - wc
                a = log "word index" wordIndex
            in { fullText         = pState.fullText
               , viewDims         = pState.viewDims
               , currentPage      = page
               , wordIndex        = wordIndex
               , pageWordCount    = wc
               }
        Swipe NoSwipe -> pState

emptyState = stringToState Server.defaultText <| viewHelper (600, 300)

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
