module UI where

import Time (Time, every, second, millisecond, timestamp)
import Maybe as M
import Html
import Graphics.Element (Element, container, relative, absolute, midTopAt, heightOf)
import List as L
import Touch
import Text
import Signal as S
import Signal ((<~), (~), Signal)
import Server
import Window
import Utils
import Keyboard
import Style

type alias WindowDimensions = (Int, Int)

type alias ViewDimensions = { fullWidth    : Int
                            , fullHeight   : Int
                            , textWidth    : Int
                            , textHeight   : Int
                            , linesPerPage : Int
                            }

type alias ViewState = { pageWordCount  : Int
                       , view           : Element
                       , viewDimensions : ViewDimensions
                       }

scene : Html.Html -> ViewDimensions -> Element
scene page viewDimensions =
    let renderTextView = Html.toElement viewDimensions.textWidth
                                        viewDimensions.textHeight
        horizontalMiddle = absolute <| viewDimensions.fullWidth // 2
        topMargin = absolute Style.viewTopMargin
        position = midTopAt horizontalMiddle topMargin
        fullContainer = container viewDimensions.fullWidth
                                  viewDimensions.fullHeight
                                  position
    in  fullContainer <| renderTextView page

viewHelper : WindowDimensions -> ViewDimensions
viewHelper (w, h) = let linesPerPage = Style.linesPerPage h
                    in { fullWidth = w
                       , fullHeight = h
                       , textWidth = min (w - 30) 650
                       , textHeight = linesPerPage * Style.lineHeight
                       , linesPerPage = linesPerPage
                       }

initialSetupSignal : Signal ()
initialSetupSignal = S.map Utils.toUnit << S.dropRepeats
                                        << S.foldp (\_ _ -> 1) 0
                                        <| every (10 * millisecond)

currentViewDimensions : Signal ViewDimensions
currentViewDimensions =
    let cues = S.mergeMany [ S.map Utils.toUnit Window.dimensions
                           , S.map Utils.toUnit initialSetupSignal ]
    in S.sampleOn cues <| S.map viewHelper Window.dimensions

type UserInput = Gesture GestureType
               | SetText String

userInput : Signal UserInput
userInput = S.mergeMany [ S.map SetText Server.textContent
                        , S.map Gesture gesture ]


type Update = Input UserInput | ViewChange ViewDimensions

type alias Tap = { x : Int, y : Int }

type GestureType = Next | Prev | NoGesture

gesture = S.merge arrowGesture tapGesture

arrowGesture : Signal GestureType
arrowGesture = let toGesture x = if | x < 0 -> Prev
                                    | x > 0 -> Next
                                    | otherwise -> NoGesture
               in S.map (toGesture << .x) Keyboard.arrows

tapGesture : Signal GestureType
tapGesture =
    let untappedValue : (Time, Tap, Bool)
        untappedValue = (0, { x = -1, y = -1 }, False)
        doubleTap = S.map (\(x,y,z) -> z) <| S.foldp isDoubleTap untappedValue
                                          <| timestamp Touch.taps
        toGestureType tap viewDims =
            if | tap.x < (viewDims.fullWidth // 2 - 40) -> Prev
               | tap.x > (viewDims.fullWidth // 2 + 40) -> Next
               | otherwise -> NoGesture
        currentGestureType = S.map2 toGestureType Touch.taps
                                                  currentViewDimensions
    in  S.sampleOn (Utils.onFalseTrueTransition doubleTap) currentGestureType

isDoubleTap : (Time, Tap) -> (Time, Tap, Bool) -> (Time, Tap, Bool)
isDoubleTap (newTapTime, newTap) (oldTapTime, oldTap, wasDoubleTap) =
    let doubleTapMargin = 80
        maxDoubleTapInterval : Time
        maxDoubleTapInterval = 0.5 * second
    in if | wasDoubleTap -> (newTapTime, newTap, False)
          | newTapTime - oldTapTime < maxDoubleTapInterval &&
              abs (newTap.x - oldTap.x) < doubleTapMargin &&
              abs (newTap.y - oldTap.y) < doubleTapMargin ->
                            (newTapTime, newTap, True)
          | otherwise    -> (newTapTime, newTap, False)
