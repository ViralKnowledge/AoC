module Day1 (run) where

import Data.Char
import Data.List.Split

dirs :: [(Int, Int)]
dirs = [(0, 1), (1, 0), (0, -1), (-1, 0)]

start = ([(0, 0)], 0)

parse :: String -> (Int, Int)
parse = head . parseSaving

parseSaving :: String -> [(Int, Int)]
parseSaving = fst . foldl (flip move) start . splitOn ", "

firstRepeated :: Eq a => [a] -> a
firstRepeated (x:xs) =
  case elem x xs of
    True -> x
    False -> firstRepeated xs

move :: String -> ([(Int, Int)], Int) -> ([(Int, Int)], Int)
move ('R':(read -> x)) = walk x . turn True
move ('L':(read -> x)) = walk x . turn False

turn :: Bool -> ([(Int, Int)], Int) -> ([(Int, Int)], Int)
turn True (pos, d) = (pos, (d + 1) `mod` length dirs)
turn False (pos, d) = (pos, (d - 1) `mod` length dirs)

walk :: Int -> ([ (Int, Int )], Int) -> ([(Int, Int) ], Int)
walk l ((px, py):ps, d)
  | l > 0 = walk (l - 1) ((px + dx, py + dy):(px, py):ps, d)
  | l <= 0 = ((px, py):ps, d)
  where
    (dx, dy) = dirs !! d

numBlocks :: (Int, Int) -> Int
numBlocks (x, y) = abs x + abs y

run :: IO ()
run = print $ numBlocks . firstRepeated . reverse $ parseSaving input
  where parsed = parse input

input = "R3, L5, R2, L1, L2, R5, L2, R2, L2, L2, L1, R2, L2, R4, R4, R1, L2, L3, R3, L1, R2, L2, L4, R4, R5, L3, R3, L3, L3, R4, R5, L3, R3, L5, L1, L2, R2, L1, R3, R1, L1, R187, L1, R2, R47, L5, L1, L2, R4, R3, L3, R3, R4, R1, R3, L1, L4, L1, R2, L1, R4, R5, L1, R77, L5, L4, R3, L2, R4, R5, R5, L2, L2, R2, R5, L2, R194, R5, L2, R4, L5, L4, L2, R5, L3, L2, L5, R5, R2, L3, R3, R1, L4, R2, L1, R5, L1, R5, L1, L1, R3, L1, R5, R2, R5, R5, L4, L5, L5, L5, R3, L2, L5, L4, R3, R1, R1, R4, L2, L4, R5, R5, R4, L2, L2, R5, R5, L5, L2, R4, R4, L4, R1, L3, R1, L1, L1, L1, L4, R5, R4, L4, L4, R5, R3, L2, L2, R3, R1, R4, L3, R1, L4, R3, L3, L2, R2, R2, R2, L1, L4, R3, R2, R2, L3, R2, L3, L2, R4, L2, R3, L4, R5, R4, R1, R5, R3"
