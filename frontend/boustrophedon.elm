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

type alias AppState = String

state : Signal AppState
state = let req = S.map (\x -> Http.get (serverUrl ++ "texts/"  ++ x)) fileName
            response = Http.send <| req
            getContent : Http.Response String -> String
            getContent response = case response of
                Http.Success str -> str
                Http.Waiting     -> "waiting."
                Http.Failure _ _ -> "http request failed."
        in S.map getContent response

lineHeight = 24
charPerLine = 80

parBreak : String
parBreak = String.fromList <| L.repeat charPerLine ' '

boustrophedon : String -> RenderState -> RenderState
boustrophedon str (reverseState, elList) =
    let classes =  classList [ ("fulljustify", True)
                             , ("reverse", reverseState)
                             ]
        nextEl = p [ classes ] [ text str ]
        nextLineState = if | str == parBreak -> False
                           | otherwise       -> not reverseState
    in (nextLineState, nextEl :: elList)

type alias ViewDimensions = { fullContainerWidth : Int
                            , fullContainerHeight : Int
                            }

viewHelper : (Int, Int) -> ViewDimensions
viewHelper (w, h) = { fullContainerWidth = w
                    , fullContainerHeight = h
                    }

currentViewDimensions : Signal ViewDimensions
currentViewDimensions = viewHelper <~ Window.dimensions

toParLines : Int -> List Char -> List (List Char)
toParLines n xs =
    let pad len ys = ys ++ L.repeat (len - L.length ys) ' '
    in case xs of
         [] -> []
         xs -> pad n (take n xs) :: (toParLines n <| drop n xs)

paragraphToLines : String -> List String
paragraphToLines par =
    let charList = '¶' :: ' ' :: String.toList par
        charLineList = toParLines charPerLine charList
    in (L.map String.fromList charLineList)

main : Signal Element
main = scene <~ currentViewDimensions
              ~ state

scene : ViewDimensions -> AppState -> Element
scene viewDims content =
    let nonEmptyLines : String -> List String
        nonEmptyLines = filter (not << String.isEmpty) << String.lines
        txtLines : List String -> List Html
        txtLines = snd << foldr boustrophedon (False, [])
                       << L.concatMap paragraphToLines
        getLineElements = txtLines << nonEmptyLines
        fullContainer = container viewDims.fullContainerWidth
                                  viewDims.fullContainerHeight
                                  middle
        renderTextView = toElement (viewDims.fullContainerWidth - 10)
                                   viewDims.fullContainerHeight
        textWidth = min viewDims.fullContainerWidth 660
        containerDivProps = style [ ("width", (toString textWidth) ++ "px")
                                  , ("margin", "0 auto")
                                  ]
        textView : List Html -> Html
        textView = div [containerDivProps]
        textViewEl = renderTextView << textView <| getLineElements content
    in  fullContainer textViewEl
