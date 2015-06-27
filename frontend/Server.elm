module Server where

import Signal as S
import Http
import Maybe exposing (Maybe(Just, Nothing))
import Utils
import Task exposing (Task)

serverUrl  : String
serverUrl = "http://localhost:8000/"

textList : S.Mailbox String
textList = S.mailbox ""

fileName : S.Mailbox String
fileName = S.mailbox ""

-- TODO do a drop updates thing so this doesn't happen twice
textContent : S.Mailbox String
textContent = S.mailbox ""
