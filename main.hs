import Data.List

-- point with x,y coordinates
data Point = P Int Int
  deriving(Show,Eq)

-- tarp consisting of a line connecting two points
data Tarp = T Point Point
  deriving(Show,Eq)

-- complete input:
-- Interval (a,b)
-- N° of tarps: n
-- List of tarps
data Input = I (Int,Int) Int [Tarp]
  deriving(Show,Eq,Read)


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
-- 6. Check:

upmostTarp :: [Tarp] -> Tarp
upmostTarp ts = head $ sortBy

overlap :: Tarp -> Tarp -> Bool
overlap (T (P a1 a2) (P b1 b2)) (T (P))
