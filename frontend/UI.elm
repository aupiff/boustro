module UI where

import Time exposing (Time, every, second, millisecond, timestamp)
import Html
import Graphics.Element exposing (Element, container, relative,
                                  absolute, midTopAt, heightOf, empty, flow, down)
import Graphics.Element
import Graphics.Input
import List as L
import Touch
import Signal as S
import Server
import Char
import Text
import Window
import Utils
import Keyboard
import Style
import Model
import Server exposing (fileName)
import Debug exposing (log)

type alias WindowDimensions = (Int, Int)

type alias ViewDimensions = { fullWidth    : Int
                            , fullHeight   : Int
                            , textWidth    : Int
                            , textHeight   : Int
                            , linesPerPage : Int
                            }

menuButton : String -> Element
menuButton = let buttonContainer = container 400 80 Graphics.Element.middle
             in buttonContainer << Graphics.Element.centered << Text.style Style.menuStyle << Text.fromString


menuScene : List Model.TextPart -> ViewDimensions -> Element
menuScene ts viewDimensions =
    let toSelectionButton tp = Graphics.Input.clickable (S.message fileName.address (tp.path)) <| menuButton tp.title
        textButtons = flow down <| L.map toSelectionButton ts
        fullContainer = container viewDimensions.fullWidth
                                  viewDimensions.fullHeight
                                  Graphics.Element.middle
    in fullContainer <| textButtons

textScene : Html.Html -> ViewDimensions -> Element
textScene page viewDimensions =
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

currentViewDimensions : Signal ViewDimensions
currentViewDimensions =
    let cues = S.mergeMany [ S.map Utils.toUnit Window.dimensions
                           , Utils.initialSetupSignal ]
    in S.sampleOn cues <| S.map viewHelper Window.dimensions

type UserInput = Gesture GestureType
               | SetText String
               | SummonMenu (Maybe String)

showMenu : Signal (Maybe String)
showMenu = S.constant (Just "fuck")
           -- let menuKey = Utils.onTrueUpdate <| Keyboard.space -- Keyboard.isDown <| Char.toCode 'm'
           --     menuCues = S.mergeMany [ menuKey, S.map Utils.toUnit Server.textListM.signal ]
           -- in S.sampleOn menuCues Server.textListM.signal

userInput : Signal UserInput
userInput = S.mergeMany [ S.map SetText Server.textContent
                        , S.map SummonMenu showMenu
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
