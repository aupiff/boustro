import List (foldr, filter, (::), take, drop)
import List as L
import String
import Html (..)
import Html.Attributes (style, classList)
import Graphics.Element (..)
import Dict
import Http
import Signal as S
import Signal ((<~), (~), Signal)
import Window
import UI (..)
import Model (..)
import Utils
import Typography

serverUrl = "http://192.168.1.212:8000/"

fileName : Signal String
fileName = S.constant "jaures.txt"

stringToState : String -> ViewDimensions -> AppState
stringToState str viewDims =
    let linesPerPage = floor <| toFloat viewDims.textHeight / lineHeight
        txtLines = Typography.typesetLines viewDims.textWidth str
        groupN n xs  = case xs of
                         [] -> []
                         _  -> take n xs :: (groupN n <| drop n xs)
        pageWidth = viewDims.textWidth
        pages = L.map (div []) <| groupN linesPerPage txtLines
    in  { currentPage = L.head pages
        , priorPages  = []
        , futurePages = L.tail pages
        }

nextState : InputData -> AppState -> AppState
nextState (userInput, viewDimensions) pState =
    case userInput of
        SetText str -> stringToState str viewDimensions
        Swipe Next  ->
            if | L.isEmpty pState.futurePages -> pState
               | otherwise ->
                     { currentPage = L.head pState.futurePages
                     , priorPages  = pState.currentPage :: pState.priorPages
                     , futurePages = L.tail pState.futurePages
                     }
        Swipe Prev  ->
            if | L.isEmpty pState.priorPages -> pState
               | otherwise ->
                     { currentPage = L.head pState.priorPages
                     , priorPages  = L.tail pState.priorPages
                     , futurePages = pState.currentPage :: pState.futurePages
                     }
        Swipe NoSwipe -> pState

emptyState = { currentPage  = (text "empty")
             , priorPages   = []
             , futurePages  = []
             }

appState : Signal AppState
appState = S.foldp nextState emptyState (S.map2 (,) userInput currentViewDimensions)

userInput : Signal UserInput
userInput = S.mergeMany [ S.map SetText textContent
                        , S.map Swipe swipe
                        ]

textContent : Signal String
textContent = let req = S.map (\x -> Http.get (serverUrl ++ "texts/"  ++ x)) fileName
                  response = Http.send <| req
                  getContent : Http.Response String -> String
                  getContent response = case response of
                      Http.Success str -> str
                      Http.Waiting     -> "waiting."
                      Http.Failure _ _ -> "http request failed."
              in S.map getContent response

scene : ViewDimensions -> AppState -> Element
scene viewDims appState =
    let renderTextView = toElement viewDims.textWidth viewDims.textHeight
        fullContainer = container viewDims.fullContainerWidth
                                  viewDims.fullContainerHeight
                                  middle
    in  fullContainer <| renderTextView appState.currentPage

main : Signal Element
main = scene <~ currentViewDimensions
              ~ appState
