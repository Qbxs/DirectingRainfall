import Data.List

-- point with x,y coordinates
type Point = (Int,Int)

-- interval from a to b (isomorph to Point)
type Range = (Int,Int)

-- tarp consisting of a line connecting two points
data Tarp = T Point Point
  deriving(Show,Eq)

-- complete input:
-- Range of vineyard (a,b)
-- N° of tarps n
-- List of tarps ts
data Input = I Range Int [Tarp]
  deriving(Show)


-- Algorithm∷
-- 1. Find upmost tarp (= tarp which has no tarp above it at any point)
-- 2. Is it in the interval?
--    - Yes: Cost to reach = 0
--    - No: Cost to reach = inf
-- 3. Find next upmost tarp:
-- 4. Is it (a) in the interval without any tarp above
--    OR (b) is it beneath the lower point of an upper tarp t?
--    - (a) Cost to reach = 0
--    - (b) Cost to reach = t
--    - Neither: if in interval of t, then cost to reach = t + 1
--    - else: cost to reach = inf
-- 5. Repeat 3 and 4 until there are no tarps left
-- 6. Check: Does it land in the interval?

upmostTarp :: [Tarp] -> Tarp
upmostTarp ts = undefined

-- Check whether two tarps overlap and tell the overlapping range
overlap :: Tarp -> Tarp -> Maybe Range
overlap (T (a,_) (b,_)) (T (c,_) (d,_))
    | (min a b) <= (min c d) && (max a b) >= (max c d) = Just (min c d,max c d)
    | (min a b) >= (min c d) && (max a b) <= (max c d) = Just (min a b,max a b)
    | (min a b) <= (min c d) && (max a b) >= (min c d) = Just (min c d,max a b)
    | (min a b) >= (min c d) && (min a b) <= (max c d) = Just (min a b,max c d)
    | otherwise                                        = Nothing
