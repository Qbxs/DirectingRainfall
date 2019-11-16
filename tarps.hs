module Tarps where

import Data.List
import InputParser (Input(..), Range, Point, Tarp(..))

-- Check whether two ranges overlap and return overlapping range
overlap :: Range -> Range -> Maybe Range
overlap (a,b) (c,d) | a <= c && b >= d = Just (c,d)
                    | a >= c && b <= d = Just (a,b)
                    | a <= c && b >= c = Just (c,b)
                    | a >= c && a <= d = Just (a,d)
                    | otherwise        = Nothing


tarpRange :: Tarp -> Range
tarpRange (T (a,_) (b,_)) = (min a b,max a b)


-- is t1 topologically above t2?
-- =(will be hit first vertically)
upper :: Tarp -> Tarp -> Bool
upper t1@(T (a1,a2) (b1,b2)) t2@(T (c1,c2) (d1,d2))
  = case overlap (tarpRange t1) (tarpRange t2) of
    Nothing    -> a2 >= c2
    Just (x,y) -> a2 >= d2 ||
                  if c2 >= b2 then False else
               -- here: all cases for which t1 overlaps t2 partially
               -- consider all combinations of {a,b} x {c,d} => 8
                     x == a1 && y == c1 && a2 >= c2
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
                  || x == d1 && y == c1 && b2 >= d2


quicksort, maxSort :: (a -> a -> Bool) -> [a] -> [a]
-- this does not do the trick :(
quicksort p []     = []
quicksort p (x:xs) = lesser ++ [x] ++ greater
               where lesser = quicksort p [a | a <- xs, not $ p x a]
                     greater = quicksort p [a | a <- xs, p x a]

-- less eficient but safe sort (isUpper is not transitive)
maxSort p [] = []
maxSort p l  = (fst $ maxList l):maxSort p (snd $ maxList l)
         where maxList [x]    = (x,[])
               maxList (x:xs) = if p x (fst $ maxList xs)
                                then (x,(fst $ maxList xs):(snd $ maxList xs))
                                else (fst $ maxList xs,x:(snd $ maxList xs))

-- lowest first
sortInput :: Input -> [Tarp]
sortInput (Input _ _ ts) = reverse $ maxSort upper ts


-- simplified tarp after sort without y-coordinates: (x1,x2)
-- if x1<x2 then tarp points to the left else to the right
data SimpleTarp = S Range Orientation
  deriving (Show, Eq)
data Orientation = L | R
  deriving (Show,Eq)

simplify :: Tarp -> SimpleTarp
simplify (T (x1,_) (x2,_)) = S (min x1 x2, max x1 x2) $ if x1 < x2 then L else R

-- cost can be an integer or nothing (=unreachable)
type Cost = Maybe Int

-- new type for tarps, split into intervals with cost to reach
type WeightedTarp = (SimpleTarp,[(Int,Cost)])

weigh :: Range -> [SimpleTarp] -> [WeightedTarp]
weigh _     []                 = []
weigh (a,b) ((S (x1,x2) o):xs) = []



-- TODO: implement a topsort to find out the order of reachable tarps (x)
-- Steps: - Look for highest Tarp (= tarp with highest upper point) which is
--          also in our [a,b] interval
--        -> First Element of topsorted tarps (ignore everything before)
--        - Add tarps in a graph together with minimal cost to reach
--        - when finished add [a,b] interval as final node + connect accordingly


-- main :: IO()
-- main = do
--   i <- readFile "inputFiles/input3.hs"
--   (print . inputParser . lexer) i
--   (print . (map simplify) . sortInput . inputParser . lexer) i
