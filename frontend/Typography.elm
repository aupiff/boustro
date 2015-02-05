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
boustro : Html.Html -> (List Html.Html, Bool) -> (List Html.Html, Bool)
boustro h (hs, reverseState) =
    let divStyle =  style [ ("height", toString Style.lineHeight ++ "px") ]
        styles = if | reverseState -> [ divStyle , Style.reverseStyle ]
                    | otherwise    -> [ divStyle ]
        nextH = Html.div styles [ h ]
        nextLineState = not reverseState
    in (nextH :: hs, nextLineState)

-- pageStyle is necessary to keep bottom indicator in the same position
--toPage : Float -> Paragraph -> Html.Html
--toPage h = let pageStyle = style [ ("height", toString h ++ "px")
--                                 , ("font-size", toString Style.fontHeight ++ "px") -- TODO remove this, shouldn't be necessary
--                                 , ("overflow", "hidden") ]
--         in Html.div [ pageStyle ] << L.reverse << fst << L.foldl boustro ([], False)
--
wordsPerLine : List Item -> Int
wordsPerLine = L.length << L.filter (not << isSpring)

wordCount : List (List Item) -> Int
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
        pagePar = par0 maxPageText
    in (Html.p [] [ Html.text "not yet implemented"], 0)

strWidth : String -> Float
strWidth str = let txtElement = Text.rightAligned << Text.style Style.textStyle
                                                  <| Text.fromString str
               in toFloat <| widthOf txtElement

wordListToItems : List String -> DocumentText
wordListToItems words =
        let toItem word = Box (strWidth word) <| Html.div [ Style.mainTextStyle ]
                                                          [ Html.text word ]
        in L.intersperse (Spring 4 3 2) <| L.map toItem words

newLine : Item -> List Line -> List Line
newLine w ls = [ w ] :: ls

addToLine : Item -> List Line -> List Line
addToLine w (l :: ls) = (w :: l) :: ls

pars : DocumentText -> List Paragraph
pars = let nextWord : Item -> List Paragraph -> List Paragraph
           nextWord w ps = L.map (newLine w) ps ++ L.map (addToLine w) ps
       in L.foldr nextWord (Spring 0 0 0)

paragraphBadness : Float -> Paragraph -> Float
paragraphBadness lineWidth ls = L.sum <| L.map (badness lineWidth) ls

par0 : DocumentText -> Paragraph
par0 = minWith paragraphBadness << L.filter (L.all fits) << pars

minWith : (a -> comparable) -> List a -> a
minWith f = L.foldl1 (\x p -> if | f x > f p -> x
                                 | otherwise -> p)

-- knuth recommends 100 * | r_j ^ 3 |, but what could that 100 possibly do?
badness : Float -> Line -> Float
badness lineWidth l =
    let adjRatio = adjustmentRatio lineWidth l
       -- 10000 is a stand in for infinity, should I just use inifinity?
    in if | adjRatio < -1 -> 10000
          | otherwise     -> abs <| adjRatio ^ 3

fits : Float -> Line -> Bool
fits lineWidth ls = (adjustmentRatio lineWidth ls) > -1

adjustmentRatio : Float -> List Item -> Float
adjustmentRatio lineWidth hs =
    let lineWidth = itemListWidth hs
        springs = L.filter isSpring hs
        widthDifference = lineWidth - lineWidth
    in if | lineWidth > lineWidth ->
            let shrinkability = L.sum << L.map (\(Spring _ _ z) -> z) <| springs
            in widthDifference / shrinkability
          | lineWidth < lineWidth ->
            let stretchability = L.sum << L.map (\(Spring _ y _) -> y) <| springs
            in widthDifference / stretchability
          | lineWidth == lineWidth -> 0

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
