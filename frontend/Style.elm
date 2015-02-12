module Style where

import Color
import Html
import Text
import Html.Attributes (style)
import Svg (svg, rect, circle)
import Svg.Attributes (version, x, y, cx, cy, r, fill, width, height, viewBox)
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
                                                <| Text.fromString "test"
             in Graphics.Element.heightOf txtElement

progressBarHeight = 8
viewTopMargin = 10
minBottomMargin = 8
fontHeight = 17

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
        circleX = (toFloat circleTravelWidth * ratio) + 4
        circleIndicator = circle [ cx <| toString circleX, cy "4", r "4", fill "black" ] []
        barHeight = toString 4
        leftBar = rect [ x "0",
                         y barHeight,
                         height "1",
                         width (toString <| max 0 <| circleX - 6 ),
                         fill "black" ] []
        rightBar = rect [ x (toString <| min circleTravelWidth <| ceiling <| circleX + 6 )
                        , y barHeight
                        , height "1"
                        , width (toString <| max 0 <| circleTravelWidth - floor circleX - 6 )
                        , fill "black" ] []
    in  divWrapper [ svgWrapper [ circleIndicator, leftBar, rightBar ] ]
