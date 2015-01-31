import List as L
import Signal as S
import Signal ((<~), (~), Signal)
import Graphics.Element (Element, empty)
import UI (..)
import Server
import Model (..)
import Utils
import Debug (log)
import Typography

stringToState : String -> ModelState
stringToState str = { fullText  = Typography.strToWordArray str
                    , wordIndex = 0 }

viewFromModelAndDims : ModelState -> ViewDimensions -> ViewState
viewFromModelAndDims modelState viewDimensions =
    let (page, wc) = Typography.typesetPage modelState viewDimensions
        view = scene page viewDimensions
    in { pageWordCount = wc
       , view = view
       , viewDimensions = viewDimensions }

update : Update -> (ModelState, ViewState) -> (ModelState, ViewState)
update update (modelState, viewState) =
    case update of
        ViewChange viewDims -> (modelState,
                                viewFromModelAndDims modelState viewDims)
        Input (SetText str) ->
                  let newModelState = stringToState str
                      newViewState = viewFromModelAndDims newModelState viewState.viewDimensions
                  in (newModelState, newViewState)
        Input (Swipe Next) ->
                  let idx = modelState.wordIndex + viewState.pageWordCount
                      newModelState = { modelState | wordIndex <- idx }
                      newViewState = viewFromModelAndDims newModelState viewState.viewDimensions
                  in (newModelState, newViewState)
        Input (Swipe Prev) ->
                  let (page, wc) = Typography.typesetPrevPage modelState viewState.viewDimensions
                      newModelState = { modelState | wordIndex <- modelState.wordIndex - wc }
                      view = scene page viewState.viewDimensions
                      newViewState = { viewState | pageWordCount <- wc
                                                 , view <- view }
                  in (newModelState, newViewState)
        Input (Swipe NoSwipe) -> (modelState, viewState)

emptyState = ( stringToState Server.defaultText
             , { pageWordCount = 0
               , view = empty
               , viewDimensions = viewHelper (300, 300) } )

appState : Signal (ModelState, ViewState)
appState = let input = (S.map Input userInput)
               viewChange = (S.map ViewChange currentViewDimensions)
           in S.foldp update emptyState <| S.merge input viewChange

userInput : Signal UserInput
userInput = S.mergeMany [ S.map SetText Server.textContent
                        , S.map Swipe swipe ]

main : Signal Element
main = S.map (.view << snd) appState
