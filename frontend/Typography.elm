module Typography where

import String
import Html
import Html.Attributes (style)
import Svg (svg, rect, circle)
import Svg.Attributes (version, x, y, cx, cy, r, fill, width, height, viewBox)
import Graphics.Element (widthOf, heightOf)
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
import Style
import Debug (log)

type Item = Box Float Html.Html      -- width, representation
          | Spring Float Float Float -- width, strechability, shrinkability
          | Penalty Float Float Bool -- TODO fill this in

-- divStyle necessary for even spacing on mobile devices TODO figure out why!
boustro : Html.Html -> (List Html.Html, Bool) -> (List Html.Html, Bool)
boustro h (hs, reverseState) =
    let divStyle =  style [ ("height", toString Style.lineHeight ++ "px") ]
        styles = if | reverseState -> [ divStyle , Style.reverseStyle ]
                    | otherwise    -> [ divStyle ]
        nextH = Html.div styles [ h ]
        nextLineState = not reverseState
    in (nextH :: hs, nextLineState)

-- pageStyle is necessary to keep bottom indicator in the same position
toPage : Int -> List Html.Html -> Html.Html
toPage h = let pageStyle = style [ ("height", toString h ++ "px")
                                 , ("font-size", toString Style.fontHeight ++ "px") -- TODO remove this, shouldn't be necessary
                                 , ("overflow", "hidden") ]
         in Html.div [ pageStyle ] << L.reverse << fst << L.foldl boustro ([], False)

wordsPerLine : List Item -> Int
wordsPerLine = L.length << L.filter (not << isSpring)

wordCount : List (List Item) -> Int
wordCount hs = (L.sum <| L.map wordsPerLine hs)

maxWordsOnPage : UI.ViewDimensions -> Int
maxWordsOnPage viewDims = viewDims.linesPerPage * viewDims.textWidth // 30

-- TODO remove magic numbers
progressSVG : Int -> Int -> UI.ViewDimensions -> Html.Html
progressSVG currentWord totalWords viewDims =
    let svgWidth = min 200 <| viewDims.textWidth // 2
        leftMargin = viewDims.textWidth // 2 - svgWidth
        circleTravelWidth = svgWidth - 8
        divWrapper = Html.div [ style [ ("margin", "auto")
                                      , ("width", toString svgWidth ++ "px") ] ]
        svgStyle = style [ ("width", toString svgWidth ++ "px")
                         , ("height", toString Style.progressBarHeight ++ "px") ]
        svgWrapper = svg [ svgStyle, version "1.1", viewBox <| "0 0 " ++ toString svgWidth ++ " " ++ toString Style.progressBarHeight]
        ratio = toFloat currentWord / toFloat totalWords
        circleX = (toFloat circleTravelWidth * ratio) + 4
        circleIndicator = circle [ cx <| toString circleX, cy "4", r "4", fill "black" ] []
        barHeight = toString 4
        leftBar = rect [ x "0",
                         y barHeight,
                         height "1",
                         width (toString <| max 0 <| circleX - 6 ),
                         fill "black" ] []
        rightBar = rect [ x (toString <| min circleTravelWidth <| ceiling <| circleX + 6 )
                        , y barHeight
                        , height "1"
                        , width (toString <| max 0 <| circleTravelWidth - floor circleX - 6 )
                        , fill "black" ] []
    in  divWrapper [ svgWrapper [ circleIndicator, leftBar, rightBar ] ]

typesetPage : ModelState -> UI.ViewDimensions -> (Html.Html, Int)
typesetPage state viewDims =
    let maxWords = min (maxWordsOnPage viewDims + state.wordIndex) state.textLength
        -- TODO probably don't have to chang this from array to LIST!!!
        wordList = Array.toList <| Array.slice state.wordIndex maxWords
                                                               state.fullText
        itemList = wordListToItems wordList
        floatTextWidth = toFloat viewDims.textWidth
        justifyForView = justifyItems viewDims.linesPerPage floatTextWidth
        (hs, lastLineItems) = L.foldl justifyForView ([], []) itemList
        a = log "adj ratios" <| L.map (adjustmentRatio floatTextWidth) hs
        htmlList = let fullLines = L.map (justifyLine floatTextWidth) hs
                   in if | not <| L.isEmpty lastLineItems ->
                              unjustifyLine lastLineItems :: fullLines
                         | otherwise -> fullLines
        page = toPage viewDims.textHeight << L.reverse <| htmlList
        progressBar = progressSVG state.wordIndex state.textLength viewDims
        wc = wordCount <| lastLineItems :: hs
    in (Html.div [] [ page, progressBar ], wc)

prevPageWordCount : ModelState -> UI.ViewDimensions -> Int
prevPageWordCount state viewDims =
    let maxWords = max 0 <| state.wordIndex - (maxWordsOnPage viewDims)
        wordList = Array.toList <| Array.slice maxWords state.wordIndex state.fullText
        itemList = wordListToItems wordList
        justifyForView = justifyItems viewDims.linesPerPage <| toFloat viewDims.textWidth
        (hs, lastLineItems) = L.foldr justifyForView ([], []) itemList
    in wordCount <| lastLineItems :: hs

strWidth : String -> Float
strWidth str = let txtElement = Text.rightAligned << Text.style Style.textStyle
                                                  <| Text.fromString str
               in toFloat <| widthOf txtElement

wordListToItems : List String -> List Item
wordListToItems words =
        let toItem word = Box (strWidth word) <| Html.div [ Style.mainTextStyle ]
                                                          [ Html.text word ]
        in L.intersperse (Spring 4 2 2) <| L.map toItem words

adjustmentRatio : Float -> List Item -> Float
adjustmentRatio optimalLineWidth hs =
    let lineWidth = itemListWidth hs
        springs = L.filter isSpring hs
        widthDifference = optimalLineWidth - lineWidth
    in if | lineWidth > optimalLineWidth ->
            let shrinkability = L.sum << L.map (\(Spring _ _ z) -> z) <| springs
            in widthDifference / shrinkability
          | lineWidth < optimalLineWidth ->
            let stretchability = L.sum << L.map (\(Spring _ y _) -> y) <| springs
            in widthDifference / stretchability
          | lineWidth == optimalLineWidth -> 0

itemWidth : Item -> Float
itemWidth i = case i of
    Box w _      -> w
    Spring w _ _ -> w
    otherwise    -> 0

itemHtml : Item -> Html.Html
itemHtml item =
    let spanStyle : Float -> Html.Attribute
        spanStyle w = style [ ("width", toString w ++ "px")
                            , ("display", "inline-block") ]
    in case item of
            Box w h   -> Html.span [spanStyle w] [h]
            Spring w _ _ -> Html.span [spanStyle w] []
            otherwise -> Html.div [] []

itemListWidth : List Item -> Float
itemListWidth = L.sum << L.map itemWidth

isSpring : Item -> Bool
isSpring item = case item of
    Spring _ _ _ -> True
    otherwise    -> False

toSpring : Float -> Item
toSpring w = Spring w 0 0

justifyLine : Float -> List Item -> Html.Html
justifyLine lineWidth is =
    let cleanList = L.filter (not << isSpring) is
        widthToAdd = lineWidth - itemListWidth cleanList
        numberSprings = L.length cleanList - 1
        springWidth = widthToAdd / toFloat numberSprings
        widthsToAdd = L.repeat numberSprings springWidth
        springs = L.map toSpring widthsToAdd
        items = Utils.interleave cleanList springs
    in Html.div [] << L.map itemHtml <| items

unjustifyLine : List Item -> Html.Html
unjustifyLine = Html.div [] << L.map itemHtml << L.reverse

justifyItems : Int -> Float -> Item -> (List (List Item), List Item) -> (List (List Item), List Item)
justifyItems numLines lineWidth item (hs, is) =
    if | L.length hs == numLines -> (hs, [])
       | otherwise -> let currentWidth = itemListWidth (item :: is)
                      in if | currentWidth > lineWidth ->
                               let nextLine = L.reverse is
                                   nextIs = if | isSpring item -> []
                                               | otherwise -> [item]
                               in (nextLine :: hs, nextIs)
                            | otherwise -> (hs, item :: is)
