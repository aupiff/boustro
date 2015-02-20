module Model where

import Array
import List as L
import String
import Graphics.Element (Element, empty)

type alias TextPart = { title : String
                      , path  : String
                      }

type ModelState = EmptyModel
                | MenuModel MenuModelData
                | TextModel TextModelData

type alias TextModelData = { fullText       : Array.Array String
                           , wordIndex      : Int
                           , pageWordCount  : Int
                           , view           : Element
                           }

type alias MenuModelData = { texts : List TextPart
                           , view : Element
                           }

strToWordArray : String -> Array.Array String
strToWordArray str = let txtLines = L.filter (not << String.isEmpty) << String.lines <| str
                         paragraphPrefix str = "¶ " ++ str
                         singleParText = String.join " " << L.map paragraphPrefix <| txtLines
                     in  Array.fromList <| String.words singleParText

modelToView model = case model of
    EmptyModel -> empty
    MenuModel data -> data.view
    TextModel data -> data.view
