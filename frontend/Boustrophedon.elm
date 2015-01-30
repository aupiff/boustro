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
    let tempState : AppState
        tempState = { fullText         = Typography.strToWordArray str
                    , viewDims         = viewDims
                    , currentPage      = text ""
                    , wordIndex        = 0
                    , pageWordCount    = 0
                    }
        (page, wc) = Typography.typesetPage tempState.wordIndex tempState
    in { tempState | currentPage <- page
                   , pageWordCount <- wc }

nextState : InputData -> AppState -> AppState
nextState userInput pState =
    case userInput of
        SetText str -> stringToState str pState.viewDims
        ViewUpdate dims ->
            let tempState = { pState | viewDims <- dims }
                (page, wc) = Typography.typesetPage pState.wordIndex tempState
                a = log "word index" pState.wordIndex
            in { tempState | currentPage <- page
                           , pageWordCount <- wc }
        Swipe Next ->
            let wordIndex = pState.wordIndex + pState.pageWordCount
                (page, wc) = Typography.typesetPage wordIndex pState
                a = log "word index" wordIndex
            in { pState | currentPage <- page
                        , wordIndex <- wordIndex
                        , pageWordCount <- wc }
        Swipe Prev ->
            let (page, wc) = Typography.typesetPrevPage pState.wordIndex pState
                wordIndex = pState.wordIndex - wc
                a = log "word index" wordIndex
            in { pState | wordIndex <- pState.wordIndex - wc
                        , currentPage <- page
                        , pageWordCount <- wc }
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
