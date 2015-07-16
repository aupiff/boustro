module Server where

import Signal as S
import Model exposing (TextPart)

serverUrl : String
serverUrl = "http://boustro.com/"

textList : List TextPart
textList = [ { title = "The Roots of Honour", path = "ruskin.txt" }
           , { title = "The Veins of Wealth", path = "ruskin2.txt" }
           , { title = "Qui Judicatis Terram", path = "ruskin3.txt" }
           , { title = "Ad Valorem", path = "ruskin4.txt" }
           ]

fileName : S.Mailbox String
fileName = S.mailbox "my-lost-city-fitzgerald.txt"

textContent : S.Mailbox String
textContent = S.mailbox ""
