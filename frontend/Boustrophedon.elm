import List (foldr, filter, (::), take, drop)
import List as L
import String
import Html (..)
import Html.Attributes (style, classList)
import Graphics.Element (..)
import Http
import Signal as S
import Signal ((<~), (~), Signal)
import Window
import UI (..)
import Model (..)
import Utils
import Typography

type alias RenderState = (Bool, List Html)

serverUrl = "http://192.168.1.212:8000/"

fileName : Signal String
fileName = S.constant "dec.txt"

stringToState : String -> ViewDimensions -> AppState
stringToState str viewDims =
    let nonEmptyLines : String -> List String
        nonEmptyLines = filter (not << String.isEmpty) << String.lines
        charPerLine = floor <| toFloat viewDims.textWidth / 8.5
        linesPerPage = floor <| toFloat viewDims.textHeight / lineHeight
        txtLines : List Html
        txtLines = snd << foldr boustrophedon (True, [])
                       << L.map String.fromList << toParLines charPerLine
                       << L.concatMap (paragraphPrefix << String.toList)
                       <| nonEmptyLines str
        groupN n xs = case xs of
                        [] -> []
                        _  -> take n xs :: (groupN n <| drop n xs)
        pages = L.map (div []) <| groupN linesPerPage txtLines
    in  { fullText    = str
        , charWidths  = L.map Typography.getWidth <| Utils.uniq <| String.toList str
        , currentPage = L.head pages
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
                     { fullText    = pState.fullText
                     , charWidths  = pState.charWidths
                     , currentPage = L.head pState.futurePages
                     , priorPages  = pState.currentPage :: pState.priorPages
                     , futurePages = L.tail pState.futurePages
                     }
        Swipe Prev  ->
            if | L.isEmpty pState.priorPages -> pState
               | otherwise ->
                     { fullText    = pState.fullText
                     , charWidths  = pState.charWidths
                     , currentPage = L.head pState.priorPages
                     , priorPages  = L.tail pState.priorPages
                     , futurePages = pState.currentPage :: pState.futurePages
                     }
        Swipe NoSwipe -> pState

emptyState = { fullText     = "empty"
             , charWidths   = []
             , currentPage  = (text "empty")
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

boustrophedon : String -> RenderState -> RenderState
boustrophedon str (reverseState, elList) =
    let classes =  classList [ ("maintext", True)
                             , ("reverse", reverseState) ]
        nextEl = p [ classes ] [ text str ]
        nextLineState = not reverseState
    in (nextLineState, nextEl :: elList)

toParLines : Int -> List Char -> List (List Char)
toParLines n xs =
    let pad len ys = ys ++ L.repeat (len - L.length ys) ' '
    in case xs of
         [] -> []
         xs -> pad n (take n xs) :: (toParLines n <| drop n xs)

paragraphPrefix : List Char -> List Char
paragraphPrefix str = ('¶' :: ' ' :: str) ++ [' ', ' ']

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
