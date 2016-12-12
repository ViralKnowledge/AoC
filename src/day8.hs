module Day8 (run) where

import Control.Lens
import Control.Lens.Traversal
import Data.Bool
import Data.Char
import Data.List
import Data.List.Split (splitOn)
import Data.Maybe
import Text.Read

data Command =
  Rect Int Int
  | Column Int Int
  | Row Int Int
  | None
  deriving Show


parse :: String -> Command
parse (stripPrefix "rect " -> Just com) = Rect (getNums com !! 0) (getNums com !! 1)
parse (stripPrefix "rotate column" -> Just com) = Column (getNums com !! 0) (getNums com !! 1)
parse (stripPrefix "rotate row" -> Just com) = Row (getNums com !! 0) (getNums com !! 1)
parse _ = None

applyCommand :: Command -> [[Bool]] -> [[Bool]]
applyCommand (Rect x y) = set (traversed . indices (<y) . traversed . indices (<x)) True
applyCommand (Row row amt) = over (traversed . index row) (reverse . shift amt . reverse)
applyCommand (Column col amt) = transpose . over (traversed . index col) (reverse . shift amt . reverse) . transpose

shift :: Int -> [Bool] -> [Bool]
shift 0 xs = xs
shift n (x:xs) = shift (n - 1) (xs ++ [x])


getNums :: String -> [Int]
getNums = mapMaybe readMaybe . filter (/= "") . words . map remchar
  where
    remchar c = if isDigit c then c else ' '

run = input >>= mapM_ (print . map (bool ' ' 'o')) . foldl (flip applyCommand) (replicate 6 (replicate 50 False))  . ((<$>) parse)

sampleInput = "rect 3x2|rotate column x=1 by 1|rotate row y=0 by 4|rotate column x=1 by 1"

input :: IO [String]
input = lines <$> readFile "input8.txt"
