module Day5 (run) where

import Control.Lens
import Crypto.Hash.MD5
import Data.ByteString.Base16
import Data.Char
import qualified Data.ByteString.Char8 as BC

findCode :: BC.ByteString -> String
findCode input = concat . take 1 . dropWhile (elem '-') $ scanl (calcHash input) "--------" num0

calcHash :: BC.ByteString -> String -> Int -> String
calcHash input out i = if (BC.take 5 hashed) == "00000" && (checkFilled . digitToInt) (BC.index hashed 5) then out & ix (digitToInt $ BC.index hashed 5) .~ (BC.index hashed 6) else out
  where
    hashed = encode . hash . BC.append input . BC.pack .show $ i
    checkFilled pos = (pos < length out) && (out !! pos == '-')

num0 = 0 : map (+1) num0

run = print $ findCode input

input = "wtnhxymk"
