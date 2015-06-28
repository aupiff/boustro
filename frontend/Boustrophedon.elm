import List as L
import Array
import Signal as S
import Graphics.Element exposing (Element, empty)
import UI exposing (..)
import Model exposing (..)
import Utils
import Typography
import Server
import Server exposing (textList, fileName, textContent)
import Task exposing (Task, andThen)
import Http
import Touch

stringToTextState : String -> ViewDimensions -> Model
stringToTextState str viewDimensions =
    let text = strToWordArray str
        wordIndex = 0
        (page, wc) = Typography.typesetPage text wordIndex viewDimensions
        view = textScene page viewDimensions
    in TextModel { fullText  = text
                 , wordIndex = wordIndex
                 , pageWordCount = wc
                 , view = view
                 }

updateView : Model -> ViewDimensions -> Model
updateView modelState viewDimensions = case modelState of
    EmptyModel -> EmptyModel
    MenuModel menuModel ->
        let view = menuScene menuModel.texts viewDimensions
        in MenuModel { menuModel | view <- view }
    TextModel textModel ->
        let (page, wc) = Typography.typesetPage textModel.fullText textModel.wordIndex viewDimensions
            view = textScene page viewDimensions
        in TextModel { textModel | pageWordCount <- wc
                                 , view <- view }

appState : Signal (ViewDimensions, Model)
appState = let input = (S.map Input userInput)
               viewChange = (S.map ViewChange currentViewDimensions)
               emptyState = (viewHelper (300, 300), EmptyModel)
           in S.foldp updateState emptyState <| S.merge input viewChange

updateState : Update -> (ViewDimensions, Model) -> (ViewDimensions, Model)
updateState update (viewDimensions, modelState) =
    case update of
        ViewChange newViewDims ->
            (newViewDims, updateView modelState newViewDims)
        Input (SetText str) ->
            (viewDimensions, stringToTextState str viewDimensions)
        Input (Gesture Next) ->
            let newModel = case modelState of
                    TextModel textModel ->
                        let textLength = Array.length textModel.fullText
                            newWordIndex = textModel.wordIndex + textModel.pageWordCount
                            idx = if newWordIndex < textLength then newWordIndex else textModel.wordIndex
                            (page, wc) = Typography.typesetPage textModel.fullText idx viewDimensions
                            view = textScene page viewDimensions
                        in TextModel { textModel | wordIndex <- idx
                                                 , pageWordCount <- wc
                                                 , view <- view }
                    otherwise -> modelState
            in (viewDimensions, newModel)
        Input (Gesture Prev) ->
            let newModel = case modelState of
                    TextModel textModel ->
                        let pwc = Typography.prevPageWordCount textModel.fullText textModel.wordIndex viewDimensions
                            idx = max (textModel.wordIndex - pwc) 0
                            (page, wc) = Typography.typesetPage textModel.fullText idx viewDimensions
                            view = textScene page viewDimensions
                        in TextModel { textModel | wordIndex <- idx
                                                 , pageWordCount <- wc
                                                 , view <- view }
                    otherwise -> modelState
            in (viewDimensions, newModel)
        Input (SummonMenu texts) ->
            let newState = MenuModel { texts = texts , view = menuScene texts viewDimensions }
            in (viewDimensions, newState)
        otherwise -> (viewDimensions, modelState)

main : Signal Element
main = S.map (modelToView << snd) appState

-- PORTS

port textContentRunner : Signal (Task Http.Error ())
port textContentRunner = S.map (\x -> Http.getString (Server.serverUrl ++ "texts/"  ++ x)
                                      `andThen` (Signal.send textContent.address)) <| fileName.signal

port menuAction : Signal (Task x ())
port menuAction =
    let toMenuAction tap viewDims =
        if | tap.y < (viewDims.fullHeight // 4)     -> Signal.send fileName.address "my-lost-city-fitzgerald.txt"
           | tap.y < (2 * viewDims.fullHeight // 4) -> Signal.send fileName.address "room-of-ones-own.txt"
           | tap.y < (3 * viewDims.fullHeight // 4) -> Signal.send fileName.address "chants.txt"
           | otherwise                              -> Signal.send fileName.address "trotsky.txt"
    in S.map2 toMenuAction Touch.taps currentViewDimensions
