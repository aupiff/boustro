import Server
import Text (plainText)
import Graphics.Element (Element)
import Signal as S
import Json.Decode (..)

textListDecoder = list textPartDecoder

textPartDecoder = object2 (,) ("title" := string) ("path" := string)

formatTextList : Maybe String -> String
formatTextList textListMaybe = case textListMaybe of
    Nothing -> "Nothing"
    Just json -> let result = decodeString textListDecoder json
                 in case result of
                     Ok val -> toString val
                     otherwise -> "Json parsing failed"

main : Signal Element
main = S.map (plainText << formatTextList) Server.textList
