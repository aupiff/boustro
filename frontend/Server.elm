module Server where

import Signal as S
import Model exposing (TextPart)

serverUrl : String
serverUrl = "http://localhost:8000/"

textList : S.Mailbox (List TextPart)
textList = S.mailbox []

fileName : S.Mailbox String
fileName = S.mailbox "my-lost-city-fitzgerald.txt"

textContent : S.Mailbox String
textContent = S.mailbox ""
