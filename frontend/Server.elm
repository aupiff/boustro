module Server where

import Signal as S
import Http

serverUrl = "http://192.168.1.212:8000/"

fileName : Signal String
fileName = S.constant "main_street.txt"

textContent : Signal String
textContent = let req = S.map (\x -> Http.get (serverUrl ++ "texts/"  ++ x)) fileName
                  response = Http.send <| req
                  getContent : Http.Response String -> String
                  getContent response = case response of
                      Http.Success str -> defaultText
                      Http.Waiting     -> defaultText
                      Http.Failure _ _ -> defaultText
              in S.map getContent response

defaultText ="""
I met a traveller from an antique land
Who said: "Two vast and trunkless legs of stone
Stand in the desert. Near them, on the sand,
Half sunk, a shattered visage lies, whose frown,
And wrinkled lip, and sneer of cold command,
Tell that its sculptor well those passions read
Which yet survive, stamped on these lifeless things,
The hand that mocked them and the heart that fed:
And on the pedestal these words appear:
'My name is Ozymandias, king of kings:
Look on my works, ye Mighty, and despair!'
Nothing beside remains. Round the decay
Of that colossal wreck, boundless and bare
The lone and level sands stretch far away."
"""
