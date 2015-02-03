module Style where

import Color
import Html
import Text
import Html.Attributes (style)
import Graphics.Element

textStyle = { typeface = [ "Georgia", "serif" ]
            , height   = Just fontHeight
            , color    = Color.black
            , bold     = False
            , italic   = False
            , line     = Nothing
            }

reverseStyle : Html.Attribute
reverseStyle = style [ ("-moz-transform", "scaleX(-1)")
                     , ("-o-transform",  "scaleX(-1)")
                     , ("-webkit-transform", "scaleX(-1)")
                     , ("transform", "scaleX(-1)")
                     , ("filter", "FlipH")
                     , ("-ms-filter", "\"FlipH\"") ]

mainTextStyle : Html.Attribute
mainTextStyle = style [ ("font-family", "Georgia, serif")
                      , ("color", "black")
                      , ("-webkit-font-smoothing", "antialiased") ]

lineHeight : Int
lineHeight = let txtElement = Text.rightAligned << Text.style textStyle
                                                <| Text.fromString "fuck"
             in Graphics.Element.heightOf txtElement
progressBarHeight = 8

viewTopMargin = 8
minBottomMargin = 8
fontHeight = 17

linesPerPage : Int -> Int
linesPerPage h = (h - viewTopMargin - progressBarHeight - minBottomMargin) // lineHeight
