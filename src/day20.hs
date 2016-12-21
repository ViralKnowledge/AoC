module Day20 where

import Data.List
import Data.List.Split
import Data.Ord

parse :: [String] -> [(Int, Int)]
parse = map (head . (\a -> zip a (drop 1 a)) . map read . splitOn "-")

reduce :: [(Int, Int)] -> [(Int, Int)]
reduce (a:[]) = [a]
reduce ((a, b):(a',b'):as) =
  case (a' < b) of
    True -> reduce ((a, max b b'):as)
    False -> (a, b):(reduce ((a', b'):as))

count :: [(Int, Int)] -> Int
count ((a, b):[]) = 4294967295 - b
count ((a, b):(a', b'):as)= (a' - b) + count ((a', b'):as)


run = input >>= print . count . reduce . sortBy (comparing fst) .  parse

input = do
  content <- readFile "input20.txt"
  return $ lines content
