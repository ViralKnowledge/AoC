{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Day22 where

import Prelude hiding (lookup)

import Control.Applicative hiding (optional, empty)
import Control.Monad
import Control.Lens
import Data.Either
import Data.Foldable (toList)
import Data.List (sortBy)
import Data.Map.Strict (Map, empty, insert, lookup)
import Data.Maybe
import Data.Ord (comparing)
import qualified Data.Sequence as DS
import qualified Data.PSQueue as PQ
import Text.Parsec (parse, try)
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String (Parser)

import Debug.Trace

data Node = Node { _pos :: (Int, Int), _size :: Int, _used :: Int, _avail :: Int } deriving Show
type NodeMap = (Map (Int, Int) Node, (Int, Int), Int)
type Move = ((Int, Int), (Int, Int), NodeMap, [(Int, Int)])

makeLenses ''Node

instance Eq Node where
  (Node pos _ _ _) == (Node pos' _ _ _) = pos == pos'

instance Ord Node where
  (Node pos _ _ _) `compare` (Node pos' _ _ _) = compare pos pos'

width = (33, 29)
dests = [(0, 13), (31, 0)]
orig = (12, 14)

printBoard :: NodeMap -> [String]
printBoard (nm, _, _) = (concat $ (\x -> x ++ " ") . show <$> [0..(fst width - 1)]) : ((\y -> foldl (\s x -> (s ++ (print $ getNode (x, y)))) (show y) [0..(fst width - 1)]) <$> [0..(snd width - 1)])
  where
    getNode p = fromJust $ lookup p nm
    print (Node _ ((> 100) -> True) _ _) = " # "
    print (Node _ _ ((== 0) -> True) _) = " _ "
    print _ = " . "

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

mapNodes :: [Node] -> NodeMap
mapNodes ns = (foldr (\n -> insert (view pos n) n) empty ns, orig, 0)

canFit :: Node -> Node -> Bool
canFit ((<= 0) . view used -> True) _ = False
canFit (view used -> a) (view avail -> b) = a <= b

countFit :: Node -> Int -> Node -> Int
countFit n c m = c + count1 (canFit n m) + count1 (canFit m n)
  where
    count1 tf = if tf then 1 else 0

doMove :: (Int, Int) -> (Int, Int) -> NodeMap -> NodeMap
doMove (x, y) (s, t) (nm, (gx, gy), d) = (insert (s, t) to' . insert (x, y) from' $ nm, if isGMove then (s, t) else (if isGMove' then (x, y) else (gx, gy)), d)
  where
    from = fromJust $ lookup (x, y) nm
    from' = from & used .~ 0 & avail .~ (view size from) & pos .~ (view pos to)
    to = fromJust $ lookup (s, t) nm
    to' = to & used +~ (view used from) & avail %~ (\a -> a - (view used from)) & pos .~ (view pos from)
    isGMove = (x, y) == (gx, gy)
    isGMove' = (s, t) == (gx, gy)
    toSize = view size to

checkMove :: Move -> Bool
checkMove ((x, y), (s, t), (nm, _, _), _) = view used from > 0 && view avail to >= view used from
  where
    from = fromJust $ lookup (x, y) nm
    to = fromJust $ lookup (s, t) nm

genMoves :: NodeMap -> [(Int, Int)] -> (Int, Int) -> DS.Seq Move
genMoves nm c ((\a -> a) -> (x, y)) = DS.fromList filtered
  where
    filtered = filter checkMove $ (\p -> ((x, y), p, nm, p:c)) <$> possibles
    possibles = filter isOnGrid $ neighbors
    neighbors = (\(dx, dy) -> (x + dx, y + dy)) <$> [(-1, 0), (1, 0), (0, -1), (0, 1)]

genAllMoves :: [(Int, Int)] -> NodeMap -> DS.Seq Move
genAllMoves c (nm, (x, y), d) = foldr (DS.><) DS.empty $ genMoves (nm, (x, y), d) c <$> checkers
  where
    ds = filter isOnGrid $ (,) <$> ((+ x) <$> [-1..1]) <*> ((+ y) <$> [-1..1])
    checkers = DS.fromList ds

isOnGrid (x, y) = x >= 0 && y >= 0 && x < fst width && y < snd width

checkDone :: NodeMap -> Bool
checkDone (nm, pos, destI) = dests !! destI == pos

solve :: PQ.PSQ Move Int -> [(Int, Int)]
solve (PQ.null -> True) = [(-1, -1)]
solve pq = if destI' == length dests then c else solve $ foldr (\m -> PQ.insert m (priority m)) queueMoves nextMoves
  where
    (from, to, nm, c) = PQ.key . fromJust $ PQ.findMin pq
    isDone = checkDone moved
    queueMoves = if isDone then PQ.empty else PQ.deleteMin pq
    nextMoves = genAllMoves c moved'
    moved = doMove from to nm
    (nm', g, destI) = moved
    moved' = if isDone then (nm', g, destI + 1) else moved
    (nm'', g', destI') = moved'

priority (_, _, (_, g, d), _) = abs (fst g - fst (dests !! d)) + abs (snd g - snd (dests !! d))

run = input >>= print . length . solve . PQ.fromList . map (\m -> m PQ.:-> (priority m)) . toList . genAllMoves [] . mapNodes . rights . map (parse node "") . drop 2
-- run = input >>= mapM_ print . printBoard . mapNodes . rights . map (parse node "") . drop 2

input = do
  content <- readFile "input22.txt"
  return $ lines content
