module Model where

import Array
import List as L
import String

type alias TextPart = { title : String
                      , path  : String
                      }

type ModelState = EmptyModel
                | MenuModel MenuModelData
                | TextModel TextModelData

type alias TextModelData = { fullText   : Array.Array String
                           , textLength : Int
                           , wordIndex  : Int
                           }

type alias MenuModelData = { texts : List TextPart }

strToWordArray : String -> Array.Array String
strToWordArray str = let txtLines = L.filter (not << String.isEmpty) << String.lines <| str
                         paragraphPrefix str = "¶ " ++ str
                         singleParText = String.join " " << L.map paragraphPrefix <| txtLines
                     in  Array.fromList <| String.words singleParText

stringToModelState : String -> ModelState
stringToModelState str = let text = strToWordArray str
                         in TextModel { fullText  = text
                                      , textLength = Array.length text
                                      , wordIndex = 0
                                      }
