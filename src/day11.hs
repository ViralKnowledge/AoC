module Day11 (run) where

import Prelude hiding (zip)

import Control.Lens
import Data.Bits
import Data.Char
import Data.List
import qualified Data.Sequence as DSQ
import Data.Maybe
import Data.Ord
import qualified Data.Set as DSE
import Data.Time.Clock
import Numeric

topFloor = 4
  
data State = State { efloor :: Int
                   , floorContents :: [Int]
                   , prevMoves :: Int
                   } deriving Show

instance Eq State where
  (==) (State f fs _) (State f' fs' _) = f == f' && fs == fs'

instance Ord State where
  compare (State a ac _) (State b bc _) = compare (a, ac) (b, bc)

data Move = Move Int deriving Eq

instance Show Move where
  show (Move m)= showIntAtBase 2 intToDigit m ""

getMove :: Move -> Int
getMove (Move m) = m

moves :: [Move]
moves = nub $ Move <$> ((+) <$> bs <*> bs)
  where
    bs = shift 1 <$> [0..(floorSize * 2 - 1)]

validMove :: State -> (Int, Move) -> Bool
validMove s m = ((from `xor` m') .&. m' == 0) && (all id $ map validFloor fc')
  where
    moved = doMove s m
    (State f fc _) = s
    (State f' fc' _) = moved
    (i, Move m') = m
    from = fc !! f
    moveMask = from `xor` m'

validFloor :: Int -> Bool
validFloor f = f > 0 && fm - fg <= 0 || fg == 0
  where
    fg = shift f (-floorSize)
    fm = f .&. (shift 1 floorSize - 1)

validMoves :: State -> [(Int, Move)]
validMoves s = filter (validMove s) floorMoves
  where
    (State floor floors _) = s
    toFloors = filter ((&&) <$> (>= 0) <*> (< topFloor)) $ (+ floor) <$> [-1, 1]
    floorMoves = (,) <$> toFloors <*> moves

doMove :: State -> (Int, Move) -> State
doMove s (i, (Move m)) = State i (fc & ix e -~ m & ix i +~ m) (p + 1)
  where
    (State e fc p) = s

checkState :: State -> Bool
checkState (State _ fc _) = last fc == (shift 1 iMax - 1) && sum (init fc) == 0
  where
    iMax = floorSize * 2

putMaybe :: (a -> Bool) -> a -> Maybe a
putMaybe f a = if f a then Just a else Nothing

-- emptyState = State 0 (bin2dec <$> ["1111111010", "0000000101", "0000000000", "0000000000"]) 0
emptyState = State 0 (bin2dec <$> ["11111111111010", "00000000000101", "00000000000000", "00000000000000"]) 0
floorSize = 7

bin2dec :: String -> Int
bin2dec = foldr (\c s -> s * 2 + c) 0 . reverse . map c2i
    where c2i c = if c == '0' then 0 else 1

solve :: (DSE.Set State, DSQ.Seq State) -> State
solve (DSE.null -> True, DSQ.null -> True) = solve (foldr DSE.insert DSE.empty nextStates, DSQ.zipWith doMove (DSQ.replicate (length nextMoves) emptyState) (DSQ.fromList nextMoves))
      where
        nextStates = DSQ.zipWith doMove (DSQ.replicate (length nextMoves) emptyState) (DSQ.fromList nextMoves)
        nextMoves = validMoves emptyState

solve (ss, (DSQ.viewl -> (s DSQ.:< sms)))
  | checkState s = s
  | otherwise =
    let
      nextStates = DSQ.zipWith doMove (DSQ.replicate (length nextMoves) s) (DSQ.fromList nextMoves)
      nextMoves = validMoves s
    in
      solve (foldr DSE.insert ss nextStates, sms DSQ.>< (DSQ.filter (\s' -> (not $ DSE.member s' ss)) $ nextStates))

run = do
  getCurrentTime >>= print
  print $ solve (DSE.empty, DSQ.empty)
  getCurrentTime >>= print
