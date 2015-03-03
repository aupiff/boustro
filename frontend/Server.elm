module Server where

import Signal as S
import Http
import Maybe (Maybe(Just, Nothing))
import Utils

serverUrl  : String
serverUrl = "http://localhost:8000/"

textUrl = S.constant <| serverUrl ++ "text"

textList : Signal (Maybe String)
textList = let response = Http.sendGet <| textUrl
               getContent : Http.Response String -> Maybe String
               getContent response = case response of
                   Http.Success str -> Just str
                   Http.Waiting     -> Nothing
                   Http.Failure _ _ -> Nothing
           in S.map getContent response

fileName : S.Channel String
fileName = S.channel ""

-- TODO do a drop updates thing so this doesn't happen twice
textContent : Signal String
textContent = let req = S.map (\x -> Http.get (serverUrl ++ "texts/"  ++ x)) <| S.subscribe fileName
                  response = Http.send <| req
                  getContent : Http.Response String -> String
                  getContent response = case response of
                      Http.Success str -> str
                      Http.Waiting     -> "waiting."
                      Http.Failure _ _ -> "loading text failed."
              in S.map getContent response
