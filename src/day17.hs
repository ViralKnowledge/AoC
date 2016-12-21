module Day17 where

import Control.Lens
import Control.Lens.Tuple
import Crypto.Hash.MD5
import Data.ByteString.Base16
import Data.Char
import Data.Maybe

import qualified Data.Sequence as S
import qualified Data.ByteString.Char8 as BC

solve :: S.Seq (BC.ByteString, (Int, Int)) -> [BC.ByteString]
solve (S.viewl -> (sf, (3, 3)) S.:< b) = sf:(solve b)
solve (S.viewl -> (soFar, (x, y)) S.:< b) = (solve (b S.>< S.fromList next))
  where
    next = map (_1 %~ (BC.append soFar) . BC.pack) . catMaybes . zipWith3 maybeNext ["U", "D", "L", "R"] [(0, -1), (0, 1), (-1, 0), (1, 0)] . BC.unpack . encode . hash $ soFar
    isOpen c = c > 'a'
    maybeNext result ((+ x) -> x', (+ y) -> y') c = if isOpen c && x' >= 0 && x' < 4 && y' >= 0 && y' < 4 then Just (result, (x', y')) else Nothing

solve (S.null -> True) = []


calcHash :: BC.ByteString -> String -> Int -> String
calcHash input out i = if (BC.take 5 hashed) == "00000" && (checkFilled . digitToInt) (BC.index hashed 5) then out & ix (digitToInt $ BC.index hashed 5) .~ (BC.index hashed 6) else out
  where
    hashed = encode . hash . BC.append input . BC.pack .show $ i
    checkFilled pos = (pos < length out) && (out !! pos == '-')


run = print $ (flip (-) (length input)) . BC.length . last . solve $ S.singleton (BC.pack input, (0, 0))

input = "bwnlcvfs"

