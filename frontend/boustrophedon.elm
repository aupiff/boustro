import List (foldr, filter, (::), take, drop)
import List as L
import String
import Html (..)
import Html.Attributes (..)
import Graphics.Element (..)
import Http
import Signal as S
import Signal ((<~), (~), Signal)
import Window

type alias RenderState = (Float, List Html)

serverUrl = "http://localhost:8000/"

fileName : Signal String
fileName = S.constant "jaures.txt"

content : Signal String
content = let req = S.map (\x -> Http.get (serverUrl ++ "texts/"  ++ x)) fileName
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


-- small reusable CSS properties
reverseProps = [ ("-moz-transform", "scaleX(-1)")
               , ("-o-transform", "scaleX(-1)")
               , ("-webkit-transform",  "scaleX(-1)")
               , ("transform", "scaleX(-1)")
               , ("filter", "FlipH")
               , ("-ms-filter", "FlipH")
               ]

mainTextProps = [ ("text-align", "justify")
                , ("width", "100%")
                ]

boustrophedon : String -> RenderState -> RenderState
boustrophedon str (lineState, elList) =
    let props = if lineState == 1 then style mainTextProps else style <| mainTextProps ++ reverseProps
        nextEl = p [ props ] [ text str ]
        nextLineState = if | str == parBreak -> 1
                           | otherwise       -> lineState * -1
    in (nextLineState, nextEl :: elList)

containerDivProps = style [ ("width", "660px")
                          , ("margin", "0 auto")
                          ]

textView : List Html -> Html
textView = div [containerDivProps]

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
              ~ content

scene : ViewDimensions -> String -> Element
scene viewDims content =
    let nonEmptyLines : String -> List String
        nonEmptyLines = filter (not << String.isEmpty) << String.lines
        txtLines : List String -> List Html
        txtLines = snd << foldr boustrophedon (1, [])
                       << L.concatMap paragraphToLines
        getLineElements = txtLines << nonEmptyLines
        fullContainer = container viewDims.fullContainerWidth
                                  viewDims.fullContainerHeight
                                  middle
        mainContainer = container 700 700 middle
        textViewEl = toElement 700 700 << textView <| getLineElements content
    in  fullContainer textViewEl
