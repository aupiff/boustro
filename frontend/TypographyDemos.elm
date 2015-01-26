import UI (..)
import Html (..)
import Graphics.Element (..)
import Signal ((<~), (~), Signal)
import Signal as S

russianAlphabet = "АБВДГЕабвгд"
englishAlphabet = "ABCDEFabcdef"

demoHtml : Html
demoHtml = 
        let
        in div [] [text russianAlphabet, text englishAlphabet]

scene : ViewDimensions -> Html -> Element
scene viewDims htm =
    let fullContainer = container viewDims.fullContainerWidth
                                  viewDims.fullContainerHeight
                                  middle
    in  fullContainer <| toElement 500 viewDims.fullContainerHeight htm

main : Signal Element
main = scene <~ currentViewDimensions
              ~ S.constant demoHtml
