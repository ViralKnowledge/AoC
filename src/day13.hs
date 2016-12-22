module Day13 (run) where

import Data.Bits
import Data.List as L
import Data.Sequence as DSQ
import Data.Set as DSE
import Data.Time.Clock

type Position = (Int, Int)

input = 1358
end = (31, 39)

isOpen :: Position -> Bool
isOpen (x, y) = even $ popCount ((x*x + 3*x + 2*x*y + y + y*y) + input)

-- solve :: Seq (Position, Int) -> Set Position -> Int
-- solve (viewl -> (s :< sms)) ps =
--   if not isOpen' || isSeen || isNeg then solve sms ps else if isAnswer then seen else solve (sms >< nextMoves) (insert (x, y) ps)
--   where
--     ((x, y), seen) = s
--     isNeg = x < 0 || y < 0
--     isOpen' = isOpen (x, y)
--     isSeen = member (x, y) ps
--     isAnswer = (x, y) == end
--     nextMoves = DSQ.fromList $ (\p -> (p, seen + 1)) <$> [(x + 1, y), (x, y + 1), (x - 1, y), (x, y - 1)]

solve :: Seq (Position, Int) -> Set Position -> Set Position
solve (DSQ.null -> True) ps = ps
solve (viewl -> (s :< sms)) ps =
  if not isOpen' || isNeg || isSeen then solve sms ps else if isAnswer then solve sms (DSE.insert (x, y) ps) else solve (sms >< nextMoves) (DSE.insert (x, y) ps)
  where
    ((x, y), seen) = s
    isNeg = x < 0 || y < 0
    isOpen' = isOpen (x, y)
    isSeen = member (x, y) ps
    isAnswer = seen == 50
    nextMoves = DSQ.fromList $ (\p -> (p, seen + 1)) <$> L.filter (not . flip member ps) [(x + 1, y), (x, y + 1), (x - 1, y), (x, y - 1)]

solve2 :: Set Position -> (Position, Int) -> Set Position
solve2 prev ((x, y), ((> 50) -> True)) = DSE.empty
solve2 prev ((x, y), seen) = unions ((DSE.singleton (x, y)):(solve2 (DSE.insert (x, y) prev)<$> nextMoves))
  where
    nextMoves = (\p -> (p, seen + 1)) <$> L.filter (not . flip member prev) [(x + 1, y), (x, y + 1), (x - 1, y), (x, y - 1)]

run = do
  getCurrentTime >>= print
  print . size $ solve (DSQ.singleton ((1, 1), 0)) DSE.empty
  getCurrentTime >>= print

