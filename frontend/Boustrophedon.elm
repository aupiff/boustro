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
        removeEmptyHead xs = if | L.isEmpty (M.withDefault [] <| Utils.listToMaybe xs) -> L.drop 1 xs
                                | otherwise -> xs
        pageWidth = viewDims.textWidth
        groupedLines = removeEmptyHead <| L.foldr (groupN linesPerPage) [] txtLines
        toPage = div [] << L.reverse << fst << L.foldl boustro ([], False)
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

emptyState = stringToState default_intro_text <| viewHelper (600, 300)

appState : Signal AppState
appState = S.foldp nextState emptyState userInput

userInput : Signal UserInput
userInput = S.mergeMany [ S.map SetText Server.textContent
                        , S.map Swipe swipe
                        , S.map ViewUpdate currentViewDimensions
                        ]

scene : ViewDimensions -> AppState -> Element
scene viewDims appState =
    let renderTextView = toElement viewDims.textWidth viewDims.textHeight
        fullContainer = container viewDims.fullContainerWidth
                                  viewDims.fullContainerHeight
                                  middle
    in  fullContainer <| renderTextView appState.currentPage

main : Signal Element
main = scene <~ currentViewDimensions
              ~ appState

default_intro_text ="""
The βουστροφηδόν Manifesto

January 30th, 2015

Dear reader, how do you your eyes feel? Are they fatigued from the hellish gymnastics required during reading? Have they skipped from the right margin, across the entire block of text, to the left one line too many? Are your eyes desperately searching for the next appropriate line without the slightest visual cue? Let boustrophedon put you at ease--there are no gynastics here, just a comfortable winding stroll through the lines of great literature. Double tap the right side of the screen to advance a page, or double tap the left side of the screen to go back a page.

Boustro etymology. comes from the greek works bous and strophe.

Reading this strange mirrored text is difficult you say? Well, sure, at first... but I'm sure Tony Hawk had difficulty riding switch for the first time, too. Later on, being able to skate with either foot forward became essential for transition skating... Everyone reading today is like a skateboarder who never learned to push with their other foot--we've been cheated of fun and comfort. We are the sorriest oxen in the world, forced to needlessly do twice as much work as necessary.

Step onto the half-pipe of life. Flip turn on your next lap.

Tolstoy reads fine in boustro: Господин этот во все время путешествия старательно избегал общения и знакомства с пассажирами. На заговариванья соседей он отвечал коротко и резко и или читал, или, глядя в окно, курил, или, достав провизию из своего старого мешка, пил чай, или закусывал.

Plato, too, is a delight to read when we take it easy on our eyes: Σωκράτης κατέβην χθὲς εἰς Πειραιᾶ μετὰ Γλαύκωνος τοῦ Ἀρίστωνος προσευξόμενός τε τῇ θεῷ καὶ ἅμα τὴν ἑορτὴν βουλόμενος θεάσασθαι τίνα τρόπον ποιήσουσιν ἅτε νῦν πρῶτον ἄγοντες. καλὴ μὲν οὖν μοι καὶ ἡ τῶν ἐπιχωρίων πομπὴ ἔδοξεν εἶναι, οὐ μέντοι ἧττον ἐφαίνετο πρέπειν ἣν οἱ Θρᾷκες ἔπεμπον.

So we march on, oxen against the plow, bourne ceaselessly along the eterenal es.
"""
