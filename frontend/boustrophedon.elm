import List (foldr, filter, (::), take, drop)
import List as L
import String
import Html (..)
import Html.Attributes (style, classList)
import Graphics.Element (..)
import Text (plainText)
import Http
import Signal as S
import Signal ((<~), (~), Signal)
import Window
import Touch
import Maybe as M
import Time (Time, second, timestamp)
import Utils as U

type alias RenderState = (Bool, List Html)

serverUrl = "http://192.168.1.212:8000/"

fileName : Signal String
fileName = S.constant "jaures.txt"

type alias AppState = { fullText     : String
                      , currentPage  : Html
                      , priorPages   : List Html
                      , futurePages  : List Html
                      }

type SwipeDir = Next | Prev | NoSwipe
type UserInput = Swipe SwipeDir
               | SetText String

userInput : Signal UserInput
userInput = S.mergeMany [ S.map SetText textContent
                        , S.map Swipe swipe
                        ]

type alias InputData = (UserInput, ViewDimensions)

stringToState : String -> ViewDimensions -> AppState
stringToState str viewDims =
    let nonEmptyLines : String -> List String
        nonEmptyLines = filter (not << String.isEmpty) << String.lines
        charPerLine = floor <| toFloat viewDims.textWidth / 7.25
        linesPerPage = floor <| toFloat viewDims.textHeight / 18
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
                     , currentPage = L.head pState.futurePages
                     , priorPages  = pState.currentPage :: pState.priorPages
                     , futurePages = L.tail pState.futurePages
                     }
        Swipe Prev  ->
            if | L.isEmpty pState.priorPages -> pState
               | otherwise ->
                     { fullText    = pState.fullText
                     , currentPage = L.head pState.priorPages
                     , priorPages  = L.tail pState.priorPages
                     , futurePages = pState.currentPage :: pState.futurePages
                     }
        Swipe NoSwipe -> pState

emptyState = { fullText     = "empty"
             , currentPage  = (text "empty")
             , priorPages   = []
             , futurePages  = []
             }

appState : Signal AppState
appState = S.foldp nextState emptyState (S.map2 (,) userInput currentViewDimensions)

touchDir : List Touch.Touch -> SwipeDir
touchDir ts = let getxOrDefault = M.withDefault 0 << M.map .x
                  firstTouch = getxOrDefault << U.listToMaybe <| ts
                  lastTouch  = getxOrDefault << U.listToMaybe << L.reverse <| ts
                  touchDist : Int
                  touchDist = firstTouch - lastTouch
              in  if | touchDist > 20 -> Prev
                     | touchDist < 20 -> Next
                     | otherwise      -> NoSwipe

type alias Tap = { x : Int, y : Int }

swipe : Signal SwipeDir
swipe = let untappedValue : (Time, Tap, Bool)
            untappedValue = (0, { x = -1, y = -1 }, False)
            doubleTap = S.map (\(x,y,z) -> z) <| S.foldp isDoubleTap untappedValue <| timestamp Touch.taps
            toSwipeDir tap viewDims = if | tap.x < (floor <| (toFloat viewDims.fullContainerWidth / 2) - 10) -> Prev
                                         | tap.x > (floor <| (toFloat viewDims.fullContainerWidth / 2) + 10) -> Next
                                         | otherwise -> NoSwipe
        in  S.sampleOn (U.onFalseTrueTransition doubleTap) <| S.map2 toSwipeDir Touch.taps currentViewDimensions

isDoubleTap : (Time, Tap) -> (Time, Tap, Bool) -> (Time, Tap, Bool)
isDoubleTap (newTapTime, newTap) (oldTapTime, oldTap, wasDoubleTap) =
    let doubleTapMargin : Int
        doubleTapMargin = 40
        maxDoubleTapInteval : Time
        maxDoubleTapInteval = 0.5 * second
    in if | wasDoubleTap -> (newTapTime, newTap, False)
          | newTapTime - oldTapTime < maxDoubleTapInteval &&
              abs (newTap.x - oldTap.x) < doubleTapMargin &&
              abs (newTap.y - oldTap.y) < doubleTapMargin ->
                            (newTapTime, newTap, True)
          | otherwise    -> (newTapTime, newTap, False)

debug : Signal String
debug = S.map toString Touch.taps

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
    let classes =  classList [ ("fulljustify", True)
                             , ("reverse", reverseState)
                             ]
        nextEl = p [ classes ] [ text str ]
        nextLineState = not reverseState
    in (nextLineState, nextEl :: elList)

type alias ViewDimensions = { fullContainerWidth : Int
                            , fullContainerHeight : Int
                            , textWidth : Int
                            , textHeight : Int
                            }

viewHelper : (Int, Int) -> ViewDimensions
viewHelper (w, h) = { fullContainerWidth = w
                    , fullContainerHeight = h
                    , textWidth = min (w - 40) 660
                    , textHeight = h - 10
                    }

currentViewDimensions : Signal ViewDimensions
currentViewDimensions = viewHelper <~ Window.dimensions

toParLines : Int -> List Char -> List (List Char)
toParLines n xs =
    let pad len ys = ys ++ L.repeat (len - L.length ys) ' '
    in case xs of
         [] -> []
         xs -> pad n (take n xs) :: (toParLines n <| drop n xs)

paragraphPrefix : List Char -> List Char
paragraphPrefix str = ('¶' :: ' ' :: str) ++ [' ', ' ']

main : Signal Element
main = scene <~ currentViewDimensions
              ~ appState
              ~ debug

scene : ViewDimensions -> AppState -> String -> Element
scene viewDims appState debug =
    let renderTextView = toElement viewDims.textWidth viewDims.textHeight
        containerDivProps = style [ ("width", (toString viewDims.textWidth) ++ "px")
                                  , ("margin", "0 auto") ]
        textView : List Html -> Html
        textView = div [containerDivProps]
        fullContainer = container viewDims.fullContainerWidth
                                  viewDims.fullContainerHeight
                                  middle
    in  layers [ fullContainer << renderTextView << textView <| [ appState.currentPage ]
               , plainText debug ]
