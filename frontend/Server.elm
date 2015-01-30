module Server where

import Signal as S
import Http

serverUrl = "http://192.168.1.212:8000/"

fileName : Signal String
fileName = S.constant "jaures.txt"

textContent : Signal String
textContent = let req = S.map (\x -> Http.get (serverUrl ++ "texts/"  ++ x)) fileName
                  response = Http.send <| req
                  getContent : Http.Response String -> String
                  getContent response = case response of
                      Http.Success str -> str
                      Http.Waiting     -> default_text
                      Http.Failure _ _ -> default_text
              in S.map getContent response

default_text =""" waiting for text to justify """
