import List as L
import String
import Html (..)
import Html.Attributes (style, classList)
import Graphics.Element (..)
import Dict
import Signal as S
import Signal ((<~), (~), Signal)
import Window
import UI (..)
import Server
import Model (..)
import Utils
import Typography
import Maybe as M
import Debug (log)

boustro : Html -> (List Html, Bool) -> (List Html, Bool)
boustro h (hs, reverseState) =
    let classes = classList [ ("reverse", reverseState) ]
        nextH = div [ classes ] [ h ]
        nextLineState = not reverseState
    in (nextH :: hs, nextLineState)

stringToState : String -> ViewDimensions -> AppState
stringToState str viewDims =
    let linesPerPage = viewDims.textHeight // lineHeight
        txtLines = Typography.typesetLines viewDims.textWidth str
        groupN : Int -> a -> List (List a) -> List (List a)
        groupN n x ys = let ns = x :: (M.withDefault [] <| Utils.listToMaybe ys)
                        in if | L.length ns == n -> [] :: ns :: L.drop 1 ys
                              | otherwise        -> ns :: L.drop 1 ys
        groupedLines = L.reverse <| L.foldl (groupN linesPerPage) [] txtLines
        toPage = div [] << L.reverse << fst << L.foldr boustro ([], False)
        pages = L.map toPage groupedLines
        a = log "length of pages" <| L.map L.length groupedLines
        b = log "viewdims" viewDims
    in  { fullText    = str
        , viewDims    = viewDims
        , currentPage = L.head pages
        , priorPages  = []
        , futurePages = L.tail pages
        }

nextState : InputData -> AppState -> AppState
nextState userInput pState =
    case userInput of
        SetText str -> stringToState str pState.viewDims
        ViewUpdate dims -> stringToState pState.fullText dims
        Swipe Next  ->
            if | L.isEmpty pState.futurePages -> pState
               | otherwise ->
                     { fullText    = pState.fullText
                     , viewDims    = pState.viewDims
                     , currentPage = L.head pState.futurePages
                     , priorPages  = pState.currentPage :: pState.priorPages
                     , futurePages = L.tail pState.futurePages
                     }
        Swipe Prev  ->
            if | L.isEmpty pState.priorPages -> pState
               | otherwise ->
                     { fullText    = pState.fullText
                     , viewDims    = pState.viewDims
                     , currentPage = L.head pState.priorPages
                     , priorPages  = L.tail pState.priorPages
                     , futurePages = pState.currentPage :: pState.futurePages
                     }
        Swipe NoSwipe -> pState

emptyState = stringToState Server.default_text <| viewHelper (600, 300)

appState : Signal AppState
appState = S.foldp nextState emptyState userInput

userInput : Signal UserInput
userInput = S.mergeMany [ S.map SetText Server.textContent
                        , S.map Swipe swipe
                        , S.map ViewUpdate currentViewDimensions
                        ]

scene : AppState -> Element
scene appState =
    let viewDims = appState.viewDims
        renderTextView = toElement viewDims.textWidth viewDims.textHeight
        fullContainer = container viewDims.fullContainerWidth
                                  viewDims.fullContainerHeight
                                  middle
    in  fullContainer <| renderTextView appState.currentPage

main : Signal Element
main = scene <~ appState
