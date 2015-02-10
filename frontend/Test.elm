module Test where

import List as L

func : List Int -> List Int
func = L.foldr test []

test : Int -> List Int -> List Int
test x ps = case ps of
    [] -> [x]
    (p :: ps') -> if | x <= p -> x :: ps'
                     | otherwise -> x :: ps
