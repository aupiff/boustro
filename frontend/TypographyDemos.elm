import UI (..)
import Html (..)
import Graphics.Element (..)
import Signal ((<~), (~), Signal)
import Signal as S
import List as L
import String
import Typography

russianAlphabet = "АБВДГЕабвгд"
englishAlphabet = "ABCDEFabcdef "

demoHtml : Html
demoHtml =
        let charsOfInterest = String.toList <| russianAlphabet ++ englishAlphabet
            charWidthPairList = L.map Typography.getWidth charsOfInterest
            htmlList = L.map (text << toString) charWidthPairList
        in div [] htmlList

scene : ViewDimensions -> Html -> Element
scene viewDims htm =
    let fullContainer = container viewDims.fullContainerWidth
                                  viewDims.fullContainerHeight
                                  middle
    in  fullContainer <| toElement 500 viewDims.fullContainerHeight htm

main : Signal Element
main = scene <~ currentViewDimensions
              ~ S.constant demoHtml
