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



-- Check whether two ranges overlap and return overlapping range
overlap :: Range -> Range -> Maybe Range
overlap (a,b) (c,d) | a <= c && b >= d = Just (c,d)
                    | a >= c && b <= d = Just (a,b)
                    | a <= c && b >= c = Just (c,b)
                    | a >= c && a <= d = Just (a,d)
                    | otherwise        = Nothing


tarpRange :: Tarp -> Range
tarpRange (T (a,_) (b,_)) = (min a b,max a b)


-- Select of two tarps that which is topologically higher
-- =(will be hit first vertically)
upper :: Tarp -> Tarp -> Tarp
upper t1@(T (a1,a2) (b1,b2)) t2@(T (c1,c2) (d1,d2))
  = case overlap (tarpRange t1) (tarpRange t2) of
    Nothing    -> if a2 >= c2 then t1 else t2
    Just (x,y) -> if a2 >= d2 then t1 else
                  if c2 >= b2 then t2 else
                  if -- TODO: all overlapping cases (5)
