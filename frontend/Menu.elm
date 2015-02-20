import Server
import Text (plainText)
import Graphics.Element (Element)
import Signal as S
import Json.Decode (..)
import Result (Result(Ok, Err))

textListDecoder : Decoder (List (String, String))
textListDecoder = list textPartDecoder

textPartDecoder : Decoder (String, String)
textPartDecoder = object2 (,) ("title" := string) ("path" := string)

formatTextList : Maybe String -> Result String (List (String, String))
formatTextList textListMaybe = case textListMaybe of
    Nothing -> Err "Nothing"
    Just json -> decodeString textListDecoder json

generateMenu : Result String (List (String, String)) -> Element
generateMenu = plainText << toString

main : Signal Element
main = S.map (generateMenu << formatTextList) Server.textList
