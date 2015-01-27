module Typography where

import String
import Html (Html, text, toElement, fromElement)
import Graphics.Element (widthOf)
import List as L
import Text (plainText)
import Maybe
import Dict
import Dict (Dict)
import Utils

type Item = Box Int Html
          | Spring Int Int Int
          | Penalty Float Float Bool

nonEmptyLines : String -> List String
nonEmptyLines = L.filter (not << String.isEmpty) << String.lines

paragraphPrefix : List Char -> List Char
paragraphPrefix str = ('¶' :: ' ' :: str) ++ [' ']

typesetLines : Int -> String -> List Html
typesetLines lineWidth str =
    let toCharParagraphs = paragraphPrefix << String.toList
        charText = L.concatMap toCharParagraphs <| nonEmptyLines str
        uniqChars  = Utils.uniq charText
        charWidths = Dict.fromList << L.map getWidth <| uniqChars
        itemList = charListToItems charText charWidths
    in justifyItems itemList lineWidth

-- TODO plaintext here will have to be replaced ...
-- how to do this nicely...we style html that needs to be converted to elements...
getWidth : Char -> (Char, Int)
getWidth c = let width = widthOf <| plainText <| String.fromChar c
             in (c, width)

charListToItems : List Char -> Dict Char Int -> List Item
charListToItems chars charDict =
        let charWidth : Char -> (Int, Html)
            charWidth c = (Maybe.withDefault 0 (Dict.get c charDict), text <| String.fromChar c)
            toBox (w, html) = Box w html
            isWhiteSpace c = L.member c <| String.toList" \n\t"
            toItem c = if | isWhiteSpace c -> Spring 5 3 3
                          | otherwise      -> toBox <| charWidth c
        in L.map toItem chars

justifyItems : List Item -> Int -> List Html
justifyItems itemList lineWidth = [text "no yet implemented"]
