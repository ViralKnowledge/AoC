module Day21 where

import Prelude hiding (drop, length, take, reverse)

import Control.Lens hiding (index)
import Control.Lens.Getter
import Control.Lens.Traversal
import Data.Foldable (toList)
import Data.List hiding (drop, length, take, reverse)
import qualified Data.List as DL
import Data.Sequence
import Debug.Trace

swapP :: Seq Char -> Int -> Int -> Seq Char
swapP s x y = s & element x .~ (s ^?! element y) & element y .~ (s ^?! element x)

swapP' = swapP

swapL :: Seq Char -> Char -> Char -> Seq Char
swapL s (flip elemIndexL s -> Just x) (flip elemIndexL s -> Just y) = swapP s x y
swapL s x y = traceShow (x:y:(toList s)) $ s

swapL' = swapL

rotateS :: Seq Char -> Bool -> Int -> Seq Char
rotateS s True (flip mod (length s) -> x) = take (length s) (drop x (s >< s))
rotateS s False x = rotateS s True ((length s) - x)

rotateS' s dir = rotateS s (not dir)

rotateL :: Seq Char -> Char -> Seq Char
rotateL s (flip elemIndexL s -> Just x) = rotateS s False (1 + x + (floor (0.25 * fromIntegral x)))

rotateL' s (flip elemIndexL s -> Just 1) = rotateS s True 1
rotateL' s (flip elemIndexL s -> Just 3) = rotateS s True 2
rotateL' s (flip elemIndexL s -> Just 5) = rotateS s True 3
rotateL' s (flip elemIndexL s -> Just 7) = rotateS s True 4
rotateL' s (flip elemIndexL s -> Just 2) = rotateS s True 6
rotateL' s (flip elemIndexL s -> Just 4) = rotateS s True 7
rotateL' s (flip elemIndexL s -> Just 6) = rotateS s True 0
rotateL' s (flip elemIndexL s -> Just 0) = rotateS s True 1

reverseP :: Seq Char -> Int -> Int -> Seq Char
reverseP s x ((+ 1) -> y) = take x s >< (reverse . drop x . take y $ s) >< drop y s

reverseP' = reverseP

move :: Seq Char -> Int -> Int -> Seq Char
move s x y = insertAt y (index s x) . deleteAt x $ s

move' s x y = move s y x

runInstruction :: [String] -> Seq Char -> Seq Char
runInstruction ["swap", "position", (read -> x), "with", "position", (read -> y)] s = swapP' s x y
runInstruction ["swap", "letter", (head -> x), "with", "letter", (head -> y)] s = swapL' s x y
runInstruction ["rotate", ((== "left") -> dir), (read -> x), (stripPrefix "step" -> Just _)] s = rotateS' s dir x
runInstruction ["rotate", "based", "on", "position", "of", "letter", (head-> x)] s = rotateL' s x
runInstruction ["reverse", "positions", (read -> x), "through", (read-> y)] s = reverseP' s x y
runInstruction ["move", "position", (read -> x), "to", "position", (read-> y)] s = move' s x y
runInstruction i s = trace "error" $ fromList (concat i)

solve :: [String] -> Seq Char -> Seq Char
solve ss cs = foldl (\cs' s -> runInstruction (words s) (traceShow cs' $ cs')) cs ss

run = input >>= \is -> print $ solve (DL.reverse is) (fromList "fbgdceah")

inputSample = ["swap position 4 with position 0", "swap letter d with letter b", "reverse positions 0 through 4", "rotate left 1 step", "move position 1 to position 4", "move position 3 to position 0", "rotate based on position of letter b", "rotate based on position of letter d"]

input = do
  content <- readFile "input21.txt"
  return $ lines content
