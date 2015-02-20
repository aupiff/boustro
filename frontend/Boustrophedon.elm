import List as L
import Signal as S
import Graphics.Element (Element, empty)
import UI (..)
import Model (..)
import Utils
import Typography

-- TODO figure out how to ensure view element w/ Elm's record system
viewFromModelAndDims : ModelState -> ViewDimensions -> ViewState
viewFromModelAndDims modelState viewDimensions = case modelState of
    EmptyModel -> EmptyView { viewDimensions = viewDimensions }
    MenuModel selectionModel ->
        let view = selectionScene selectionModel viewDimensions
        in MenuView { view = view
                    , viewDimensions = viewDimensions
                    }
    TextModel textModel ->
        let (page, wc) = Typography.typesetPage textModel viewDimensions
            view = textScene page viewDimensions
        in TextView { pageWordCount = wc
                    , view = view
                    , viewDimensions = viewDimensions
                    }

appState : Signal (ModelState, ViewState)
appState = let input = (S.map Input userInput)
               viewChange = (S.map ViewChange currentViewDimensions)
               emptyState = (EmptyModel, EmptyView { viewDimensions = viewHelper (300, 300) } )
           in S.foldp updateState emptyState <| S.merge input viewChange

updateState : Update -> (ModelState, ViewState) -> (ModelState, ViewState)
updateState update (modelState, viewState) =
    case update of
        ViewChange viewDims -> (modelState, viewFromModelAndDims modelState viewDims)
        Input (SetText str) ->
            let viewDimensions = case viewState of
                    TextView textViewData -> textViewData.viewDimensions
                    MenuView menuViewData -> menuViewData.viewDimensions
                    EmptyView emptyViewData -> emptyViewData.viewDimensions
                newModelState = stringToModelState str
                newViewState = viewFromModelAndDims newModelState viewDimensions
            in (newModelState, newViewState)
        Input (Gesture Next) ->
            case (viewState, modelState) of
                (TextView textViewData, TextModel textModelData)  ->
                    let idx = textModelData.wordIndex + textViewData.pageWordCount
                    in if | idx < textModelData.textLength ->
                            let newModelState = TextModel { textModelData | wordIndex <- idx }
                                newViewState = viewFromModelAndDims newModelState textViewData.viewDimensions
                            in (newModelState, newViewState)
                          | otherwise -> (modelState, viewState)
                otherwise -> (modelState, viewState)
        Input (Gesture NoGesture) -> (modelState, viewState)
        Input (Gesture Prev) ->
            case (viewState, modelState) of
                (TextView textViewData, TextModel textModelData)  ->
                    let wc = Typography.prevPageWordCount textModelData textViewData.viewDimensions
                        idx = max (textModelData.wordIndex - wc) 0
                        newModelState = TextModel { textModelData | wordIndex <- idx }
                        newViewState = viewFromModelAndDims newModelState textViewData.viewDimensions
                    in (newModelState, newViewState)
                otherwise -> (modelState, viewState)

main : Signal Element
main = S.map (viewToElement << snd) appState
