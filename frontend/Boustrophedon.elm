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
                    , wordIndex = 0
                    }

viewFromModelAndDims : ModelState -> ViewDimensions -> ViewState
viewFromModelAndDims modelState viewDimensions =
    let (page, wc) = Typography.typesetPage modelState viewDimensions
        view = scene page viewDimensions
    in { pageWordCount = wc, view = view }

update : (UserInput, ViewDimensions) -> (ModelState, ViewState) -> (ModelState, ViewState)
update (userInput, viewDimensions) (modelState, viewState) =
    case userInput of
        SetText str ->
            let newModelState = stringToState str
                newViewState = viewFromModelAndDims newModelState viewDimensions
            in (newModelState, newViewState)
        Swipe Next ->
            let idx = modelState.wordIndex + viewState.pageWordCount
                newModelState = { modelState | wordIndex <- idx }
                newViewState = viewFromModelAndDims newModelState viewDimensions
            in (newModelState, newViewState)
        Swipe Prev ->
            let newModelState = { modelState | wordIndex <- 0 }
                newViewState = viewFromModelAndDims newModelState viewDimensions
            in (newModelState, newViewState)
        Swipe NoSwipe -> (modelState, viewState)

emptyState = ( stringToState Server.defaultText
             , { pageWordCount = 0, view = empty }
             )

appState : Signal (ModelState, ViewState)
appState = S.foldp update emptyState <| S.map2 (,) userInput currentViewDimensions

userInput : Signal UserInput
userInput = S.mergeMany [ S.map SetText Server.textContent
                        , S.map Swipe swipe
                        ]

main : Signal Element
main = S.map (.view << snd) appState
