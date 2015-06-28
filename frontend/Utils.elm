module Utils where

import List as L
import Set
import Signal as S
import Signal.Extra as SE
import Time exposing (every, millisecond)

onTrueUpdate : Signal Bool -> Signal ()
onTrueUpdate signal = SE.keepWhen signal () (S.map toUnit signal)

initialSetupSignal : Signal ()
initialSetupSignal = S.map toUnit << S.dropRepeats
                                  << S.foldp (\_ _ -> 1) 0
                                  <| every (10 * millisecond)

secondSetupSignal : Signal ()
secondSetupSignal = S.map toUnit << S.dropRepeats
                                 << S.foldp (\_ _ -> 1) 0
                                 <| every (2500 * millisecond)


minWith : (a -> comparable) -> List a -> a
minWith f (x :: xs) = L.foldl (\x p -> if | f x < f p -> x
                                          | otherwise -> p) x xs

toUnit : a -> ()
toUnit _ = ()

onFalseTrueTransition : Signal Bool -> Signal ()
onFalseTrueTransition sig =
  let storeLastTwo c (p1, p2) = (c, p1)
      lastTwo : Signal (Bool, Bool)
      lastTwo = S.foldp storeLastTwo (False, False) sig
  in  S.map toUnit <| S.filter (\(x, y) -> x && not y) (False, False) lastTwo

uniq : List comparable -> List comparable
uniq = Set.toList << Set.fromList

interleave : List a -> List a -> List a
interleave xs' ys' = case xs' of
                      [] -> ys'
                      (x :: xs) -> case ys' of
                                    [] -> xs'
                                    (y :: ys) -> x :: y :: interleave xs ys
