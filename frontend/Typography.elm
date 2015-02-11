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
type alias Paragraph = ( List Line, -- lines of paragraph
                         Float,     -- current line width
                         Float      -- badness of whole paragraph
                       )

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
toPage : Float -> List Line -> Html.Html
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

wordCount : List Line -> Int
wordCount = L.sum << L.map wordsPerLine

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
        (par, _, _) = toPar floatTextWidth maxPageText
        pagePar = L.map (justifyLine floatTextWidth) <| L.take viewDims.linesPerPage par
        page = toPage (toFloat viewDims.textHeight) pagePar
        progressBar = Style.progressSVG state.wordIndex state.textLength viewDims.textWidth
    in (Html.div [] [ page, progressBar ], wordCount pagePar)

prevPageWordCount : ModelState -> UI.ViewDimensions -> Int
prevPageWordCount state viewDims =
    let maxWords = max 0 <| state.wordIndex - maxWordsOnPage viewDims
        wordList = Array.toList <| Array.slice maxWords state.wordIndex state.fullText
        maxPageText = wordListToItems wordList
        floatTextWidth = toFloat viewDims.textWidth
        (par, _, _) = toPar floatTextWidth maxPageText
        pagePar = L.map (justifyLine floatTextWidth) <| L.take viewDims.linesPerPage par
    in  wordCount pagePar

strWidth : String -> Float
strWidth str = let txtElement = Text.rightAligned << Text.style Style.textStyle
                                                  <| Text.fromString str
               in toFloat <| widthOf txtElement

spaceWidth = 5

wordListToItems : List String -> DocumentText
wordListToItems words =
        let toItem : String -> Item
            toItem word = Box (strWidth word) <| Html.div [ Style.mainTextStyle ]
                                                          [ Html.text word ]
           -- TODO this defines the default space must be played with
        in L.intersperse (Spring spaceWidth 3 2) <| L.map toItem words

-- This will change when lineWidth becomes variable
newLine : Item -> Paragraph -> Paragraph
newLine w (ls, lineWidth, pBadness) =
    let nls = [ w ] :: ls
    in (nls, lineWidth, pBadness)

addToLine : Item -> Paragraph -> Paragraph
addToLine w ((l :: ls), lineWidth, pb) =
    let nls = (w :: l) :: ls
    in (nls, lineWidth, pb)

toPar : Float -> DocumentText -> Paragraph
toPar lineWidth =
    let headFits : Paragraph -> Bool
        headFits (par, _, _) = fits lineWidth <| L.head par -- we only check head here because
                        -- we necessarily have already checked the tail elements
        minBad : List Paragraph -> Paragraph
        minBad = Utils.minWith (paragraphBadness lineWidth)
        nextWord : Item -> List Paragraph -> List Paragraph
        nextWord w ps = L.filter headFits ( newLine w (minBad ps) :: L.map (addToLine w) ps )
    in  minBad << L.foldr nextWord [ ([ [ (Spring 0 0 0) ] ], 0, 0) ]

-- TeX checks that neighboring lines have similar adj ratios... this would be a good thing to add!
paragraphBadness : Float -> Paragraph -> Float
paragraphBadness lineWidth (ls, _, pb) = case ls of
    [_] -> 0
    (l :: _) -> pb + badness lineWidth l

-- knuth recommends 100 * | r_j ^ 3 |, but what could that 100 possibly do?
badness : Float -> Line -> Float
badness lineWidth l =
    let adjRatio = adjustmentRatio lineWidth l
       -- 10000 is a stand in for infinity, should I just use inifinity?
    in if | adjRatio < -1 -> 10000
          | otherwise     -> adjRatio ^ 2

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
