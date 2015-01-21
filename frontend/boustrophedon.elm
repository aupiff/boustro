import Graphics.Collage (..)
import Graphics.Element (..)
import Text (plainText, justified, leftAligned, rightAligned)
import Text as T
import List (foldr, filter, (::), take, drop)
import List as L
import String
import Http
import Signal as S
import Transform2D (scaleX)
import Color (grayscale, rgb)

type alias RenderState = (Float, List Element)

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

boustrophedon : String -> RenderState -> RenderState
boustrophedon str (lineState, elList) =
    let xScale = scaleX lineState
        isParagraphStart = L.any (\x -> x == '¶') <| String.toList str
        lineColor = if | isParagraphStart -> rgb 200 210 220
                       | lineState > 0    -> grayscale 0.1
                       | otherwise        -> grayscale 0.14
        background = rect 1000 lineHeight |> filled lineColor
        formText = toForm <| justified << T.monospace <| T.fromString str
        txtWidth = (String.length str) * 9
        nextEl = collage txtWidth lineHeight [
                      background
                    , groupTransform xScale [ formText ]
                 ]
        nextLineState = if | str == parBreak -> 1
                           | otherwise       -> lineState * -1
    in (nextLineState, nextEl :: elList)

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
main = let nonEmptyLines : String -> List String
           nonEmptyLines = filter (not << String.isEmpty) << String.lines
           txtLines : List String -> List Element
           txtLines = snd << foldr boustrophedon (-1, [])
                          << L.concatMap paragraphToLines
           getLineElements = txtLines << nonEmptyLines
       in  S.map (flow down << getLineElements) content
