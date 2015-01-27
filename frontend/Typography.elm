module Typography where

import String
import Html (Html, text, toElement, fromElement)
import Graphics.Element (widthOf)
import List as L
import Text (plainText)
import Maybe
import Dict
import Dict (Dict)

type Item = Box Int Html
          | Spring Float Float Float
          | Penalty Float Float Bool

-- TODO plaintext here will have to be replaced ...
-- how to do this nicely...we style html that needs to be converted to elements...
getWidth : Char -> (Char, Int)
getWidth c = let width = widthOf <| plainText <| String.fromChar c
             in (c, width)

-- TODO make all spaces, tabs, (whitespace) springs
parStringToItems : String -> Dict Char Int -> List Item
parStringToItems str charDict =
        let chars = String.toList str
            charWidth : Char -> (Int, Html)
            charWidth c = (Maybe.withDefault 0 (Dict.get c charDict), text <| String.fromChar c)
            toBox (w, html) = Box w html
        in L.map (toBox << charWidth) chars
