module Model where

import Array
import List as L
import String

type alias ModelState = { fullText  : Array.Array String
                        , wordIndex : Int
                        }

strToWordArray : String -> Array.Array String
strToWordArray str = let txtLines = L.filter (not << String.isEmpty) << String.lines <| str
                         paragraphPrefix str = "¶ " ++ str
                         singleParText = String.join " " << L.map paragraphPrefix <| txtLines
                     in  Array.fromList <| String.words singleParText

stringToModelState : String -> ModelState
stringToModelState str = { fullText  = strToWordArray str
                         , wordIndex = 0 }
