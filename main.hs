import Data.List
import InputParser

-- Algorithm:
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
                    -- here: all cases for which t1 overlaps t2 partially
                    -- consider all combinations of {a,b} x {c,d} => 8
                  if x == a1 && y == c1 && a2 >= c2
                  || x == a1 && y == d1 && b2 >= d2
                  || x == b1 && y == c1 && b2 >= c2
                  || x == b1 && y == d1 && b2 >= d2
                  || x == c1 && y == a1 && a2 >= c2
                  || x == c1 && y == b1 && b2 >= c2
                  || x == d1 && y == a1 && a2 >= c2
                  || x == d1 && y == b1 && b2 >= d2
                    -- here: all cases of total overlap where t1 is higher
                  || x == a1 && y == b1 && a2 >= c2
                  || x == b1 && y == a1 && a2 >= c2
                  || x == c1 && y == d1 && b2 >= d2
                  || x == d1 && y == c1 && b2 >= d2 then t1 else t2


-- use these babies to sort all tarps
isUpper :: Tarp -> Tarp -> Bool
isUpper t1 t2 | upper t1 t2 == t1 = True
              | otherwise         = False

-- this does not do the trick :(
quicksort :: (a -> a -> Bool) -> [a] -> [a]
quicksort p []     = []
quicksort p (x:xs) = lesser ++ [x] ++ greater
             where lesser = quicksort p [a | a <- xs, p a x]
                   greater = quicksort p [a | a <- xs, p x a]

sortInput :: Input -> [Tarp]
sortInput (Input _ _ ts) = quicksort isUpper ts


-- new type for tarps, split into intervals with cost to reach
type WeightedTarp = (Tarp,[(Range,Int)])

-- orientation of tarp
data Orientation = Left | Right



-- TODO: implement a topsort to find out the order of reachable tarps
-- Steps: - Look for highest Tarp (= tarp with highest upper point) which is
--          also in our [a,b] interval
--        -> First Element of topsorted tarps (ignore everything before)
--        - Add tarps in a graph together with minimal cost to reach
--        - when finished add [a,b] interval as final node + connect accordingly



main :: IO()
main = do
  i <- readFile "input.hs"
  (print . inputParser . lexer) i
  (print . sortInput . inputParser . lexer) i
