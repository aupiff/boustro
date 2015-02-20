import Server
import List as L
import Text (plainText)
import Graphics.Element (..)
import Graphics.Input (..)
import Signal as S
import Json.Decode (..)
import Result (Result(Ok, Err))
import Debug (log)

textListDecoder : Decoder (List (String, String))
textListDecoder = list textPartDecoder

textPartDecoder : Decoder (String, String)
textPartDecoder = object2 (,) ("title" := string) ("path" := string)

formatTextList : Maybe String -> Result String (List (String, String))
formatTextList textListMaybe = case textListMaybe of
    Nothing -> Err "Nothing"
    Just json -> decodeString textListDecoder json

generateButton : (String, String) -> Element
generateButton (str, _) =
    let el = plainText str
    in el |> clickable (S.send selection str)

generateMenu : Result String (List (String, String)) -> Element
generateMenu result = case result of
    Ok val -> flow down <| L.map generateButton val
    Err msg -> plainText <| toString msg

selection : S.Channel String
selection = S.channel "no selection"

scene : Element -> Element -> Element
scene e1 e2 = flow down [e1, e2]

main : Signal Element
main = let menu = S.map (generateMenu << formatTextList) Server.textList
           selectionText = S.map plainText (S.subscribe selection)
       in S.map2 scene menu selectionText
