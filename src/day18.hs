module Day18 where

import Data.List

nextRow :: [Bool] -> [Bool]
nextRow r = foldr ((:) . isTrap) [] trips
  where
    expanded = False:r ++ [False]
    trips = foldr (zipWith (:)) (repeat []) . take  3 . tails $ expanded
    isTrap [True, _, False] = True
    isTrap [False, _, True] = True
    isTrap _ = False

solve :: Int -> [Bool] -> [[Bool]]
solve n init = scanl nextRow' init (replicate n 0)
  where
    nextRow' bs _ = nextRow bs


parse :: String -> [Bool]
parse = reverse . foldl (\bs c -> (c == '^'):bs) []

run = print . sum . map (length . filter (not . id)) . solve (399999) $ parse "^.^^^..^^...^.^..^^^^^.....^...^^^..^^^^.^^.^^^^^^^^.^^.^^^^...^^...^^^^.^.^..^^..^..^.^^.^.^......."
