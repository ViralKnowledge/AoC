module Day15 where

import Prelude hiding (length, map, reverse, take)

import Data.Bool
import Data.Foldable (toList)
import Data.Sequence

maxLength = 35651584

showSeq :: Seq Bool -> String
showSeq = toList . fmap (bool '0' '1')

readSeq :: String -> Seq Bool
readSeq = fmap (== '1') . fromList

generateData :: Seq Bool -> Seq Bool
generateData (maybeTake maxLength -> Just a) = a
generateData a = generateData $ a >< singleton False >< (not <$> reverse a)

calculateChecksum :: Seq Bool -> Seq Bool
calculateChecksum (maybeOdd -> Just checksum) = checksum
calculateChecksum a = calculateChecksum $ booleanXor <$> chunks
  where
    chunks = chunksOf 2 a

booleanXor :: Seq Bool -> Bool
booleanXor (viewl -> a :< (viewl -> b :< empty)) = a == b

maybeTake :: Int -> Seq a -> Maybe (Seq a)
maybeTake n a = if length taken == n then Just taken else Nothing
  where taken = take n a

maybeOdd :: Seq a -> Maybe (Seq a)
maybeOdd a = if mod (length a) 2 /= 0 then Just a else Nothing

run = print $ showSeq . calculateChecksum . generateData $ readSeq "10011111011011001"
