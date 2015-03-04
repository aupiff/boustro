module Style where

import Color
import Html
import Text
import Html.Attributes (style)
import Svg (svg, rect, circle)
import Svg.Attributes (version, x, y, cx, cy, r, fill, width, height, viewBox)
import Graphics.Element

menuStyle : Text.Style
menuStyle = { typeface = [ "Georgia", "serif" ]
            , height   = Just menuFontHeight
            , color    = Color.blue
            , bold     = False
            , italic   = False
            , line     = Nothing
            }

textStyle : Text.Style
textStyle = { typeface = [ "Georgia", "serif" ]
            , height   = Just fontHeight
            , color    = Color.black
            , bold     = False
            , italic   = False
            , line     = Nothing
            }

-- reverseStyle : Html.Attribute
reverseStyle = [ ("-moz-transform", "scaleX(-1)")
               , ("-o-transform",  "scaleX(-1)")
               , ("-webkit-transform", "scaleX(-1)")
               , ("transform", "scaleX(-1)")
               , ("filter", "FlipH")
               , ("-ms-filter", "\"FlipH\"") ]

mainTextStyle : Html.Attribute
mainTextStyle = style [ ("font-family", "Georgia, serif")
                      , ("color", "black")
                      , ("-webkit-font-smoothing", "antialiased")
                      , ("margin-top", toString textMargins ++ "px")
                      , ("margin-bottom", toString textMargins ++ "px")
                      ]

lineHeight : Int
lineHeight = let txtElement = Text.rightAligned << Text.style textStyle
                                                <| Text.fromString "test"
             in Graphics.Element.heightOf txtElement + textMargins * 2

progressBarHeight = 8
viewTopMargin = 10
minBottomMargin = 8
fontHeight = 17
menuFontHeight = 30
textMargins = 1

linesPerPage : Int -> Int
linesPerPage h = (h - viewTopMargin - progressBarHeight - minBottomMargin) // lineHeight

-- TODO remove magic numbers
progressSVG : Int -> Int -> Int -> Html.Html
progressSVG currentWord totalWords textWidth =
    let svgWidth = min 200 <| textWidth // 2
        leftMargin = textWidth // 2 - svgWidth
        circleTravelWidth = svgWidth - 8
        divWrapper = Html.div [ style [ ("margin", "auto")
                                      , ("width", toString svgWidth ++ "px") ] ]
        svgStyle = style [ ("width", toString svgWidth ++ "px")
                         , ("height", toString progressBarHeight ++ "px") ]
        svgWrapper = svg [ svgStyle, version "1.1", viewBox <| "0 0 " ++ toString svgWidth ++ " " ++ toString progressBarHeight]
        ratio = toFloat currentWord / toFloat totalWords
        radius = 3
        strRadius = toString radius
        circleX = (toFloat circleTravelWidth * ratio) + radius
        circleIndicator = circle [ cx <| toString circleX, cy "4", r strRadius, fill "black" ] []
        barHeight = toString 4
        leftBar = rect [ x "0",
                         y barHeight,
                         height "1",
                         width (toString <| max 0 <| circleX - radius - 2 ),
                         fill "black" ] []
        rightBar = rect [ x (toString <| min circleTravelWidth <| ceiling <| circleX + radius + 2 )
                        , y barHeight
                        , height "1"
                        , width (toString <| max 0 <| circleTravelWidth - floor circleX - radius - 2 )
                        , fill "black" ] []
    in  divWrapper [ svgWrapper [ circleIndicator, leftBar, rightBar ] ]
