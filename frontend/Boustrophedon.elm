import List as L
import Signal as S
import Signal ((<~), (~), Signal)
import Graphics.Element (Element, empty)
import UI (..)
import Server
import Model (..)
import Utils
import Typography

viewFromModelAndDims : ModelState -> ViewDimensions -> ViewState
viewFromModelAndDims modelState viewDimensions =
    let (page, wc) = Typography.typesetPage modelState viewDimensions
        view = scene page viewDimensions
    in { pageWordCount = wc
       , view = view
       , viewDimensions = viewDimensions }

emptyState = ( stringToModelState Server.defaultText
             , { pageWordCount = 0
               , view = empty
               , viewDimensions = viewHelper (300, 300) }
             )

appState : Signal (ModelState, ViewState)
appState = let input = (S.map Input userInput)
               viewChange = (S.map ViewChange currentViewDimensions)
           in S.foldp update emptyState <| S.merge input viewChange

update : Update -> (ModelState, ViewState) -> (ModelState, ViewState)
update update (modelState, viewState) =
    case update of
        ViewChange viewDims -> (modelState, viewFromModelAndDims modelState viewDims)
        Input (SetText str) ->
            let newModelState = stringToModelState str
                newViewState = viewFromModelAndDims newModelState viewState.viewDimensions
            in (newModelState, newViewState)
        Input (Gesture Next) ->
            let idx = modelState.wordIndex + viewState.pageWordCount
            in if | idx < modelState.textLength ->
                     let newModelState = { modelState | wordIndex <- idx }
                         newViewState = viewFromModelAndDims newModelState viewState.viewDimensions
                     in (newModelState, newViewState)
                  | otherwise -> (modelState, viewState)
        Input (Gesture Prev) ->
            let wc = Typography.prevPageWordCount modelState viewState.viewDimensions
                idx = max (modelState.wordIndex - wc) 0
                newModelState = { modelState | wordIndex <- idx }
                newViewState = viewFromModelAndDims newModelState viewState.viewDimensions
            in (newModelState, newViewState)
        Input (Gesture NoGesture) -> (modelState, viewState)

main : Signal Element
main = S.map (.view << snd) appState
