module Day19 where

import Prelude hiding (length, splitAt, null, zip, replicate)

import Data.Sequence

solve :: (Seq (Int, Int), Seq (Int, Int)) -> Int
solve ((null -> True), (viewl -> winner :< (null -> True))) = fst winner
solve ((viewl -> (e :< ls)),(viewl -> (loser :< rs))) = solve $ splitAt (floor $ (fromIntegral $ length ls + length rs + 1) * 0.5) (ls >< rs >< singleton winner)
  where
    winner = (fst e, snd e + snd loser)

run = print $ solve $ splitAt (floor $ (fromIntegral input) * 0.5) elves

elves = zip (fromList [0..(input - 1)]) (replicate input 1)
input = 3018458
