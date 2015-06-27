module Server where

import Signal as S
import Http
import Maybe exposing (Maybe(Just, Nothing))
import Utils
import Task exposing (Task)
import Model exposing (TextPart)

serverUrl  : String
serverUrl = "http://localhost:8000/"

textList : S.Mailbox (List TextPart)
textList = S.mailbox []

fileName : S.Mailbox String
fileName = S.mailbox "my-lost-city-fitzgerald.txt"

-- TODO do a drop updates thing so this doesn't happen twice
textContent : S.Mailbox String
textContent = S.mailbox ""
