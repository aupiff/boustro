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

type alias RenderState = (Bool, List Html)

serverUrl = "http://192.168.1.65:8000/"

fileName : Signal String
fileName = S.constant "jaures.txt"

type alias AppState = { fullText     : String
                      , currentPage  : Html
                      , priorPages   : List Html
                      , futurePages  : List Html
                      }

type UserInput = NextPage
               | PrevPage
               | SetText String

userInput : Signal UserInput
userInput = S.mergeMany [ S.map SetText textContent ]

type alias InputData = (UserInput, ViewDimensions)

stringToState : String -> ViewDimensions -> AppState
stringToState str viewDims =
    let nonEmptyLines : String -> List String
        nonEmptyLines = filter (not << String.isEmpty) << String.lines
        charPerLine = floor <| toFloat viewDims.textWidth / 7.25
        txtLines : List Html
        txtLines = snd << foldr boustrophedon (True, [])
                       << L.map String.fromList << toParLines charPerLine
                       << L.concatMap (paragraphPrefix << String.toList)
                       <| nonEmptyLines str
    in  { fullText    = str
        , currentPage = L.head txtLines
        , priorPages  = []
        , futurePages = L.tail txtLines
        }

nextState : InputData -> AppState -> AppState
nextState (userInput, viewDimensions) pState =
    case userInput of
        SetText str -> stringToState str viewDimensions
        NextPage    -> pState
        PrevPage    -> pState

emptyState = { fullText     = "empty"
             , currentPage  = (text "empty")
             , priorPages   = []
             , futurePages  = []
             }

appState : Signal AppState
appState = S.foldp nextState emptyState (S.map2 (,) userInput currentViewDimensions)

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

scene : ViewDimensions -> AppState -> Element
scene viewDims appState =
    let renderTextView = toElement viewDims.textWidth viewDims.textHeight
        containerDivProps = style [ ("width", (toString viewDims.textWidth) ++ "px")
                                  , ("margin", "0 auto") ]
        textView : List Html -> Html
        textView = div [containerDivProps]
        fullContainer = container viewDims.fullContainerWidth
                                  viewDims.fullContainerHeight
                                  middle
    in  fullContainer << renderTextView << textView <| [ appState.currentPage ]
