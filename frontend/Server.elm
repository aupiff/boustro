module Server where

import Signal as S
import Http
import Debug (log)

serverUrl = "http://localhost:8000/"

textList : Signal String
textList = let response = Http.sendGet <| S.constant <| serverUrl ++ "text"
               getContent : Http.Response String -> String
               getContent response = case response of
                   Http.Success str -> let _ = log "text List" str
                                       in str
                   Http.Waiting     -> "waiting."
                   Http.Failure _ _ -> "loading text failed."
           in S.map getContent response

fileName : Signal String
fileName = S.constant "apology.txt"

-- TODO do a drop updates thing so this doesn't happen twice
textContent : Signal String
textContent = let req = S.map (\x -> Http.get (serverUrl ++ "texts/"  ++ x)) fileName
                  response = Http.send <| req
                  getContent : Http.Response String -> String
                  getContent response = case response of
                      Http.Success str -> str
                      Http.Waiting     -> "waiting."
                      Http.Failure _ _ -> "loading text failed."
              in S.map getContent response
