module Typography where

import String
import Html (Html, text, toElement, fromElement)
import Graphics.Element (widthOf)
import List as L
import Text (plainText)

type Item = Box Int Html
          | Spring Float Float Float
          | Penalty Float Float Bool

-- TODO plaintext here will have to be replaced
getWidth : Char -> (Int, Char)
getWidth c = let width = widthOf <| plainText <| String.fromChar c
             in (width, c)

charToItem : Char -> Item
charToItem c =
        let charHtml = text <| String.fromChar c
            charElement = toElement charHtml
            charWidth = widthOf charElement
        in Box charWidth charHtml

parStringToItems : String -> List Item
parStringToItems str =
        let chars = String.toList str
        in L.map charToItem chars
