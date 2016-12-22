{-# LANGUAGE TemplateHaskell #-}

module Day22 where

import Prelude hiding (lookup)

import Control.Applicative hiding (optional, empty)
import Control.Monad
import Control.Lens
import Data.Either
import Data.Map.Strict (Map, empty, insert, lookup)
import Data.Maybe
import qualified Data.Sequence as DS
import Text.Parsec (parse, try)
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String (Parser)

import Debug.Trace

data Node = Node { _pos :: (Int, Int), _size :: Int, _used :: Int, _avail :: Int } deriving Show
type NodeMap = Map (Int, Int) Node
type Move = ((Int, Int), (Int, Int), NodeMap, Int)

makeLenses ''Node

width = 3

-- Parsing

lexeme :: Parser a -> Parser a
lexeme p = do
  x <- p
  whitespace
  return x

whitespace :: Parser ()
whitespace = void $ many $ oneOf "\n\t "

position :: Parser (Int, Int)
position = do
  x <- string "/dev/grid/node-x" *> many1 digit
  y <- string "-y" *> many1 digit
  return (read x, read y)

dataSize :: Parser Int
dataSize = read <$> many1 digit <* char 'T'

node :: Parser Node
node = do
  pos <- lexeme position
  size <- lexeme dataSize
  used <- lexeme dataSize
  avail <- lexeme dataSize
  return $ Node pos size used avail

-- Solve

mapNodes :: [Node] -> Map (Int, Int) Node
mapNodes = foldr (\n -> insert (view pos n) n) empty

canFit :: Node -> Node -> Bool
canFit ((<= 0) . view used -> True) _ = False
canFit (view used -> a) (view avail -> b) = a <= b

countFit :: Node -> Int -> Node -> Int
countFit n c m = c + count1 (canFit n m) + count1 (canFit m n)
  where
    count1 tf = if tf then 1 else 0

doMove :: (Int, Int) -> (Int, Int) -> NodeMap -> NodeMap
doMove (x, y) (s, t) nm = insert (s, t) to' . insert (x, y) from' $ nm
  where
    from = fromJust $ lookup (x, y) nm
    from' = from & used .~ 0 & avail .~ (view size from)
    to = fromJust $ lookup (s, t) nm
    to' =
      case from of
        (Node (0, 32) s u a) -> Node (0, 32) toSize (u + (view used from)) (toSize - u - (view used from))
        (Node _ _ u _) -> to & used +~ u & avail %~ (\a -> a - u)
    toSize = view size to

checkMove :: Move -> Bool
checkMove ((x, y), (s, t), nm, _) = view avail to >= view used from
  where
    from = fromJust $ lookup (x, y) nm
    to = fromJust $ lookup (s, t) nm

genMoves :: NodeMap -> Int -> (Int, Int) -> DS.Seq ((Int, Int), (Int, Int), NodeMap, Int)
genMoves nm c (x, y) = DS.fromList $ filter checkMove $ (\p -> ((x, y), p, nm, c + 1)) <$> possibles
  where
    possibles = filter (/= (x, y)) $ (,) <$> (neighborsD x) <*> (neighborsD y)
    neighborsD i = filter filterEdges $ (+ i) <$> [-1..1]
    filterEdges i = i >= 0 && i < width

genAllMoves :: Int -> NodeMap -> DS.Seq ((Int, Int), (Int, Int), NodeMap, Int)
genAllMoves c nm = foldr (DS.><) DS.empty $ genMoves nm c <$> DS.zip allPoses allPoses
  where
    allPoses = DS.fromList [0..(width - 1)]

checkDone :: NodeMap -> Bool
checkDone (lookup (0, 0) -> Just (Node (0, (== (width - 1)) -> True) _ _ _)) = True
checkDone _ = False

solve :: DS.Seq Move -> Int
solve (DS.viewl -> (_, _, checkDone -> True, c) DS.:< ms) = c
solve (DS.viewl -> (from, to, nm, c) DS.:< ms) = solve $ ms DS.>< genAllMoves c moved
  where
    moved = doMove from to nm

run = input >>= print . solve . genAllMoves 0 . mapNodes . rights . map (parse node "") . drop 2

input = do
  content <- readFile "input22.txt"
  return $ lines content
