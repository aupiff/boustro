module Typography where

import String
import Html
import Html.Attributes (style)
import Svg (svg, rect, circle)
import Svg.Attributes (version, x, y, cx, cy, r, fill, width, height)
import Graphics.Element (widthOf)
import List as L
import Array
import Array (Array)
import Text
import Color
import Maybe
import Dict
import Dict (Dict)
import Model (ModelState)
import Utils
import UI

type Item = Box Int Html.Html
          | Spring Int Int Int
          | Penalty Float Float Bool

textStyle = { typeface = [ "Georgia", "serif" ]
            , height   = Just UI.textHeight
            , color    = Color.black
            , bold     = False
            , italic   = False
            , line     = Nothing
            }

reverseStyle : Html.Attribute
reverseStyle = style [ ("-moz-transform", "scaleX(-1)")
                     , ("-o-transform",  "scaleX(-1)")
                     , ("-webkit-transform", "scaleX(-1)")
                     , ("transform", "scaleX(-1)")
                     , ("filter", "FlipH")
                     , ("-ms-filter", "\"FlipH\"") ]

mainTextStyle : Html.Attribute
mainTextStyle = style [ ("font-family", "Georgia, serif")
                      , ("color", "black")
                      , ("-webkit-font-smoothing", "antialiased") ]

boustro : Html.Html -> (List Html.Html, Bool) -> (List Html.Html, Bool)
boustro h (hs, reverseState) =
    let divStyle = style [ ("height", toString UI.lineHeight ++ "px") ]
        styles = if | reverseState -> [ reverseStyle, divStyle ]
                    | otherwise    -> [ divStyle ]
        nextH = Html.div styles [ h ]
        nextLineState = not reverseState
    in (nextH :: hs, nextLineState)

toPage : Int -> List Html.Html -> Html.Html
toPage h =  let divStyle = style [ ("height", toString h ++ "px") ]
            in Html.div [ divStyle ] << L.reverse << fst << L.foldl boustro ([], False)

wordsPerLine : List Item -> Int
wordsPerLine = L.length << L.filter (not << isSpring)

wordCount : List (List Item) -> Int
wordCount hs = (L.sum <| L.map wordsPerLine hs)

maxWordsOnPage : UI.ViewDimensions -> Int
maxWordsOnPage viewDims = viewDims.linesPerPage * viewDims.textWidth // 30

-- TODO remove magic numbers
progressSVG : Int -> Int -> Int -> Html.Html
progressSVG currentWord totalWords textWidth =
    let svgWidth = textWidth // 2
        circleTravelWidth = svgWidth - 8
        svgWrapper = svg [ version "1.1", width <| toString svgWidth, height "8" ]
        divWrapper = Html.div [ style [ ("margin", "0 auto")
                                      , ("width", toString svgWidth ++ "px") ] ]
        ratio = toFloat currentWord / toFloat totalWords
        rects = if | ratio < 0.01 -> rect [ fill "black", width "100", height "8" ] []
                   | ratio > 0.99 -> rect [ fill "black", width "100", height "8" ] []
                   | otherwise    -> rect [ fill "black", width "100", height "8" ] []
        circleX = (toFloat circleTravelWidth * ratio) + 4
        circleIndicator = circle [ cx <| toString circleX, cy "4", r "4", fill "black" ] []
        leftBar = rect [ x "0",
                         y "4",
                         height "1",
                         width (toString <| max 0 <| circleX - 6 ),
                         fill "black" ] []
        rightBar = rect [ x (toString <| min circleTravelWidth <| ceiling <| circleX + 6 )
                        , y "4"
                        , height "1"
                        , width (toString <| max 0 <| circleTravelWidth - (floor circleX) )
                        , fill "black" ] []
    in  divWrapper [ svgWrapper [ circleIndicator, leftBar, rightBar ] ]

typesetPage : ModelState -> UI.ViewDimensions -> (Html.Html, Int)
typesetPage state viewDims =
    let maxWords = min (maxWordsOnPage viewDims + state.wordIndex) state.textLength
        -- TODO probably don't have to chang this from array to LIST!!!
        wordList = Array.toList <| Array.slice state.wordIndex maxWords
                                                               state.fullText
        itemList = wordListToItems wordList
        justifyForView = justifyItems viewDims.linesPerPage viewDims.textWidth
        (hs, lastLineItems) = L.foldl justifyForView ([], []) itemList
        htmlList = let fullLines = L.map (justifyLine viewDims.textWidth) hs
                   in if | not <| L.isEmpty lastLineItems ->
                              unjustifyLine lastLineItems :: fullLines
                         | otherwise -> fullLines
        page = toPage viewDims.textHeight << L.reverse <| htmlList
        progressBar = progressSVG state.wordIndex state.textLength viewDims.textWidth
        wc = wordCount <| lastLineItems :: hs
    in (Html.div [] [ page, progressBar ], wc)

prevPageWordCount : ModelState -> UI.ViewDimensions -> Int
prevPageWordCount state viewDims =
    let maxWords = max 0 <| state.wordIndex - maxWordsOnPage viewDims
        wordList = Array.toList <| Array.slice maxWords state.wordIndex state.fullText
        itemList = wordListToItems wordList
        justifyForView = justifyItems viewDims.linesPerPage viewDims.textWidth
        (hs, lastLineItems) = L.foldr justifyForView ([], []) itemList
    in wordCount <| lastLineItems :: hs

strWidth : String -> Int
strWidth str = let txtElement = Text.rightAligned << Text.style textStyle
                                                  <| Text.fromString str
               in widthOf txtElement

wordListToItems : List String -> List Item
wordListToItems words =
        let toItem word = Box (strWidth word) <| Html.div [ mainTextStyle ]
                                                          [ Html.text word ]
        in L.intersperse (Spring 4 2 2) <| L.map toItem words

itemWidth : Item -> Int
itemWidth i = case i of
    Box w _      -> w
    Spring w _ _ -> w
    otherwise    -> 0

itemHtml : Item -> Html.Html
itemHtml item =
    let spanStyle : Int -> Html.Attribute
        spanStyle w = style [ ("width", toString w ++ "px")
                            , ("height", "16px")
                            , ("display", "inline-block") ]
    in case item of
            Box w h   -> Html.span [spanStyle w] [h]
            Spring w _ _ -> Html.span [spanStyle w] []
            otherwise -> Html.div [] []

itemListWidth : List Item -> Int
itemListWidth = L.sum << L.map itemWidth

isSpring : Item -> Bool
isSpring item = case item of
    Spring _ _ _ -> True
    otherwise    -> False

toSpring : Int -> Item
toSpring w = Spring w 0 0

lineDivStyle = style [ ("margin-bottom", toString UI.lineMarginBottom ++ "px") ]

justifyLine : Int -> List Item -> Html.Html
justifyLine lineWidth is =
    let cleanList = L.filter (not << isSpring) is
        widthToAdd = lineWidth - itemListWidth cleanList
        numberSprings = L.length cleanList - 1
        baseSpringWidth = widthToAdd // numberSprings
        remainingWidth = rem widthToAdd numberSprings
        widthsToAdd = L.repeat remainingWidth (baseSpringWidth + 1) ++ L.repeat (numberSprings - remainingWidth) baseSpringWidth
        springs = L.map toSpring widthsToAdd
        items = Utils.interleave cleanList springs
    in Html.div [ lineDivStyle ] << L.map itemHtml <| items

unjustifyLine : List Item -> Html.Html
unjustifyLine = Html.div [ lineDivStyle ] << L.map itemHtml << L.reverse

justifyItems : Int -> Int -> Item -> (List (List Item), List Item) -> (List (List Item), List Item)
justifyItems numLines lineWidth item (hs, is) =
    if | L.length hs == numLines -> (hs, [])
       | otherwise -> let currentWidth = itemListWidth (item :: is)
                      in if | currentWidth > lineWidth ->
                               let nextLine = L.reverse is
                                   nextIs = if | isSpring item -> []
                                               | otherwise -> [item]
                               in (nextLine :: hs, nextIs)
                            | otherwise -> (hs, item :: is)
