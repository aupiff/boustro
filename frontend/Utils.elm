module Utils where

import List as L
import Set
import Signal as S
import Time (every, millisecond)

initialSetupSignal : Signal ()
initialSetupSignal = S.map toUnit << S.dropRepeats
                                  << S.foldp (\_ _ -> 1) 0
                                  <| every (10 * millisecond)

listToMaybe : List a -> Maybe a
listToMaybe xs = if | L.isEmpty xs -> Nothing
                    | otherwise    -> Just <| L.head xs

minWith : (a -> comparable) -> List a -> a
minWith f = L.foldl1 (\x p -> if | f x < f p -> x
                                 | otherwise -> p)

toUnit : a -> ()
toUnit _ = ()

onFalseTrueTransition : Signal Bool -> Signal ()
onFalseTrueTransition sig =
  let storeLastTwo c (p1, p2) = (c, p1)
      lastTwo : Signal (Bool, Bool)
      lastTwo = S.foldp storeLastTwo (False, False) sig
      isFalseTrueTrans : Signal Bool
      isFalseTrueTrans = S.map (\(x, y) -> x && not y) lastTwo
  in  S.map toUnit <| S.dropWhen (S.map not isFalseTrueTrans) (False, False) lastTwo

uniq : List comparable -> List comparable
uniq = Set.toList << Set.fromList

interleave : List a -> List a -> List a
interleave xs' ys' = case xs' of
                      [] -> ys'
                      (x :: xs) -> case ys' of
                                    [] -> xs'
                                    (y :: ys) -> x :: y :: interleave xs ys
