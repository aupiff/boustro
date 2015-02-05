module Typography where

import String
import Html
import Html.Attributes (style)
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
type alias DocumentText = List Item
type alias Line = List Item
type alias Paragraph = List Line

-- divStyle necessary for even spacing on mobile devices TODO figure out why!
boustro : List Html.Html -> (List Html.Html, Bool) -> (List Html.Html, Bool)
boustro is (hs, reverseState) =
    let divStyle =  style [ ("height", toString Style.lineHeight ++ "px") ]
        styles = if | reverseState -> [ divStyle , Style.reverseStyle ]
                    | otherwise    -> [ divStyle ]
        nextH = Html.div styles is
        nextLineState = not reverseState
    in (nextH :: hs, nextLineState)

-- pageStyle is necessary to keep bottom indicator in the same position
toPage : Float -> Paragraph -> Html.Html
toPage h paragraph =
    let pageStyle = style [ ("height", toString h ++ "px")
                          , ("font-size", toString Style.fontHeight ++ "px") -- TODO remove this, shouldn't be necessary
                          , ("overflow", "hidden") ]
        htmlLines = L.map (L.map itemHtml) paragraph
    in Html.div [ pageStyle ] << L.reverse << fst << L.foldl boustro ([], False) <| htmlLines

justifyLine : Float -> Line -> Line
justifyLine lineWidth is =
    let cleanList = L.filter (not << isSpring) is
        widthToAdd = lineWidth - itemListWidth cleanList
        numberSprings = L.length cleanList - 1
        springWidth = widthToAdd / toFloat numberSprings
        springs = L.repeat numberSprings (Box springWidth <| Html.div [ Style.mainTextStyle ] [ Html.text " " ])
    in Utils.interleave cleanList springs

wordsPerLine : Line -> Int
wordsPerLine = L.length << L.filter (not << isSpring)

wordCount : Paragraph -> Int
wordCount hs = (L.sum <| L.map wordsPerLine hs)

maxWordsOnPage : UI.ViewDimensions -> Int
maxWordsOnPage viewDims = viewDims.linesPerPage * viewDims.textWidth // 30

typesetPage : ModelState -> UI.ViewDimensions -> (Html.Html, Int)
typesetPage state viewDims =
    let maxWords = min (maxWordsOnPage viewDims + state.wordIndex) state.textLength
        -- TODO probably don't have to chang this from array to LIST!!!
        wordList = Array.toList <| Array.slice state.wordIndex maxWords
                                                               state.fullText
        maxPageText = wordListToItems wordList
        floatTextWidth = toFloat viewDims.textWidth
        pagePar = L.map (justifyLine floatTextWidth) << L.take viewDims.linesPerPage <| toPar floatTextWidth maxPageText
        page = toPage (toFloat viewDims.textHeight) pagePar
    in (page, wordCount pagePar)

prevPageWordCount : ModelState -> UI.ViewDimensions -> Int
prevPageWordCount state viewDims =
    let maxWords = max 0 <| state.wordIndex - maxWordsOnPage viewDims
        wordList = Array.toList <| Array.slice maxWords state.wordIndex state.fullText
        maxPageText = wordListToItems wordList
        floatTextWidth = toFloat viewDims.textWidth
        pagePar = L.map (justifyLine floatTextWidth) << L.take viewDims.linesPerPage <| toPar floatTextWidth maxPageText
    in  wordCount pagePar

strWidth : String -> Float
strWidth str = let txtElement = Text.rightAligned << Text.style Style.textStyle
                                                  <| Text.fromString str
               in toFloat <| widthOf txtElement

wordListToItems : List String -> DocumentText
wordListToItems words =
        let toItem : String -> Item
            toItem word = Box (strWidth word) <| Html.div [ Style.mainTextStyle ]
                                                          [ Html.text word ]
        in L.intersperse (Spring 4 3 2) <| L.map toItem words

newLine : Item -> List Line -> List Line
newLine w ls = [ w ] :: ls

addToLine : Item -> List Line -> List Line
addToLine w (l :: ls) = (w :: l) :: ls

toPar : Float -> DocumentText -> Paragraph
toPar lineWidth =
    let fitH = fits lineWidth << L.head
        minBad = minWith (paragraphBadness lineWidth)
        nextWord w ps = L.filter fitH ( newLine w (minBad ps) :: L.map (addToLine w) ps )
    in  minBad << L.foldr nextWord [ [ [ (Spring 0 0 0) ] ] ]

minWith : (a -> comparable) -> List a -> a
minWith f = L.foldl1 (\x p -> if | f x < f p -> x
                                 | otherwise -> p)

paragraphBadness : Float -> Paragraph -> Float
paragraphBadness lineWidth ls = L.sum <| L.map (badness lineWidth) ls

-- knuth recommends 100 * | r_j ^ 3 |, but what could that 100 possibly do?
badness : Float -> Line -> Float
badness lineWidth l =
    let adjRatio = adjustmentRatio lineWidth l
        --a = log "adjRatio" adjRatio
       -- 10000 is a stand in for infinity, should I just use inifinity?
    in if | adjRatio < -1 -> 10000
          | otherwise     -> 100 * (abs <| adjRatio ^ 3)

fits : Float -> Line -> Bool
fits lineWidth ls = (adjustmentRatio lineWidth ls) > -1

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
