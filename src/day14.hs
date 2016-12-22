module Day14  where

import Control.Lens
import Crypto.Hash.MD5
import Data.ByteString.Base16
import Data.Char
import Data.List
import qualified Data.ByteString.Char8 as BC

findHash :: Int -> BC.ByteString -> BC.ByteString
findHash 0 h = h
findHash i h = findHash (i - 1) (encode . hash $ h)

calcHash :: BC.ByteString -> Int -> [(Char, Int)] -> [Int]
calcHash _ ((> 22065) -> True) [] = []
calcHash input i found = pads ++ calcHash input (i + 1) nextHashes
  where
    found' = filter (\(_, i') -> i' + 1000 > i && i' `notElem` pads) found
    grouped = BC.group . findHash 2017 . BC.append input . BC.pack . show $ i
    triplet = take 1 . filter ((>= 3) . BC.length) $ grouped
    nextHashes = found' ++ [(BC.head $ head triplet, i) | i <= 22065 && not (null triplet)]
    pentlets = map BC.head $ filter ((>= 5) . BC.length) grouped
    pads = map snd $ filter (\(c, _) -> elem c pentlets) found

run = print . drop 62 . sort $ calcHash "yjdafjpo" 0 []
