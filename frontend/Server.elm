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
textContent : Signal String
textContent = S.constant <| "hello there"
    -- let req = S.map (\x -> Http.get (serverUrl ++ "texts/"  ++ x)) <| fileName.signal
              -- in S.map getContent response
