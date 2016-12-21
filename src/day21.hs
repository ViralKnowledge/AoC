module Day21 where

import Prelude hiding (drop, length, take, reverse)

import Control.Lens hiding (index)
import Control.Lens.Getter
import Control.Lens.Traversal
import Data.Sequence
import Data.Sequence.Lens

swapP :: Seq Char -> Int -> Int -> Seq Char
swapP s x y = s & element x .~ (s ^?! element y) & element y .~ (s ^?! element x)

swapL :: Seq Char -> Char -> Char -> Seq Char
swapL s (flip elemIndexL s -> Just x) (flip elemIndexL s -> Just y) = swapP s x y

rotateS :: Seq Char -> Bool -> Int -> Seq Char
rotateS s False (mod (length s) -> x) = take (length s) (drop x (s >< s))
rotateS s True x = rotateS s False ((length s) - x)

rotateL :: Seq Char -> Char -> Seq Char
rotateL s (flip elemIndexL s -> Just x) = rotateS s False (1 + x + (floor (0.25 * fromIntegral x)))

reverseP :: Seq Char -> Int -> Int -> Seq Char
reverseP s x y = take x s >< (reverse . drop x . take (y-x) $ s) >< drop y s

move :: Seq Char -> Int -> Int -> Seq Char
move s x y = insertAt y (index s x) . deleteAt x $ s

runInstruction :: 

solve :: [String] -> Seq Char -> Seq Char
solve s:ss cs = 





run = print "hi"
