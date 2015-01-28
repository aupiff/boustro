module Server where

import Signal as S
import Http

serverUrl = "http://localhost:8000/"

fileName : Signal String
fileName = S.constant "boustro_intro.txt"

textContent : Signal String
textContent = let req = S.map (\x -> Http.get (serverUrl ++ "texts/"  ++ x)) fileName
                  response = Http.send <| req
                  getContent : Http.Response String -> String
                  getContent response = case response of
                      Http.Success str -> str
                      Http.Waiting     -> "waiting."
                      Http.Failure _ _ -> "http request failed."
              in S.map getContent response
