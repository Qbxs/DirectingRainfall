{-# LANGUAGE TupleSections #-}

module Tarps where

import Data.List
import Data.Tuple
import Data.Graph
import Control.Applicative
import InputParser (Input(..), Range, Point, Tarp(..))

instance Functor ((,,) a b) where
  fmap f (x,y,z) = (x,y,f z)

thd3 :: (a,b,c) -> c
thd3 (a,b,c) = c

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
    Just (x,y) -> a2 >= d2
               -- here: all cases for which t1 overlaps t2 partially
               -- consider all combinations of {a,b} x {c,d} => 8
                  || x == a1 && y == c1 && a2 > c2
                  || x == a1 && y == d1 && b2 > d2
                  || x == b1 && y == c1 && b2 > c2
                  || x == b1 && y == d1 && b2 > d2
                  || x == c1 && y == a1 && a2 > c2
                  || x == c1 && y == b1 && b2 > c2
                  || x == d1 && y == a1 && a2 > c2
                  || x == d1 && y == b1 && b2 > d2
               -- here: all cases of total overlap where t1 is higher
                  || x == a1 && y == b1 && a2 > c2
                  || x == b1 && y == a1 && a2 > c2
                  || x == c1 && y == d1 && b2 > d2
                  || x == d1 && y == c1 && b2 > d2


quicksort, maxSort :: (a -> a -> Bool) -> [a] -> [a]
-- this does not do the trick :(
quicksort p []     = []
quicksort p (x:xs) = lesser ++ [x] ++ greater
               where lesser = quicksort p [a | a <- xs, not $ p x a]
                     greater = quicksort p [a | a <- xs, p x a]

-- less eficient but safe sort (isUpper is not transitive)
maxSort p [] = []
maxSort p l  = fst (maxList l):maxSort p (snd $ maxList l)
         where maxList [x]    = (x,[])
               maxList (x:xs) = if x `p` fst (maxList xs)
                                then (x,uncurry (:) (maxList xs))
                                else (fst $ maxList xs,x:(snd $ maxList xs))



-- simplified tarp after sort without y-coordinates: (x1,x2)
-- if x1<x2 then tarp points to the left else to the right
data SimpleTarp = S Range Orientation
  deriving (Show, Eq)
data Orientation = L | R | N --left | right | neutral(vineyard)
  deriving (Show,Eq)

simplify :: Tarp -> SimpleTarp
simplify t@(T (x1,_) (x2,_)) = S (tarpRange t) $ if x1 < x2 then L else R


-- METHOD A: beam intervals down for data efficiency

-- split into all relevant intervals
split :: Range -> [SimpleTarp] -> [(SimpleTarp,[Int])]
split _     []                     = []
split (a,b) [t]                    = [(t,[a,b])]
split r     (t:w@((S (x,y) _):ts)) = let s = split r w in
                                     (t,(++) [x,y] $ snd $ head s):s

-- remove duplicates and sort intervals
clean :: (Ord b) => [(a,[b])] -> [(a,[b])]
clean = map $ fmap $ map head . group . sort


-- METHOD B: get all intervals for computation efficency

-- gives list of all relevant intervals, first arg is range as a 2-elem-list
intervals :: [Int] -> [SimpleTarp] -> [Int]
intervals = foldr $ \(S (x1,x2) _) xs -> [x1,x2] ++ xs

-- sort out intervals that are not part of the tarp
sortOut :: (SimpleTarp,[Int]) -> (SimpleTarp,[Int])
sortOut (S (x1,x2) o,[])     = (S (x1,x2) o,[])
sortOut (S (x1,x2) o,r:rs)
                 | r < x1    = sortOut (S (x1,x2) o,rs)
                 | r > x2    = (S (x1,x2) o,[])
                 | otherwise = (\(x,ys) -> (x,r:ys)) $ sortOut (S (x1,x2) o,rs)

toRanges :: [Int] -> [Range]
toRanges []           = []
toRanges [_]          = []
toRanges (a:xs@(b:_)) = (a,b):toRanges xs

-- turn around intervals if tarp is orienteted towards left
turn :: (SimpleTarp,[Range]) -> (SimpleTarp,[Range])
turn s@(S _ R,_) = reverse . map swap <$> s
turn s           = s

-- new type for tarps, split into intervals with cost to reach
type WeightedTarps = [(SimpleTarp,[WeightedRange])]
type WeightedRange = (Int,Int,Cost)
type Cost          = Maybe Int

minCost :: Cost -> Cost -> Cost
minCost (Just c) (Just d) = Just $ min c d
minCost (Just c) Nothing  = Just c
minCost Nothing  (Just d) = Just d
minCost Nothing  Nothing  = Nothing

minRange :: WeightedRange -> WeightedRange -> WeightedRange
minRange (a,b,c) (_,_,z) = (a,b,minCost c z)


incr :: (Num a) => a -> Maybe a
incr x = Just $ x + 1

-- calculate costs of an interval of a tarp based on costs of tarps above
costAbove :: SimpleTarp -> Range -> WeightedTarps -> WeightedRange
costAbove _         (a,b) []        = (a,b,Nothing)
costAbove s         r   ((_,[]):ts) = costAbove s r ts
costAbove s         (a,b) ((t,(_,_,Nothing):rs):ts) --keep looking (trivial?)
                                    = costAbove s (a,b) ((t,rs):ts)
costAbove s@(S _ R) (a,b) ((S (x1,x2) R,(r2,r1,c):rs):ts) -- R on R
              | r2 == b && r2 == x2 = minRange (a,b,c) (costAbove s (a,b) ((S (x1,x2) R,rs):ts))
              | (b,a) == (r1,r2)    = (a,b,c >>= incr)
              | otherwise           = costAbove s (a,b) ((S (x1,x2) R,rs):ts)
costAbove s@(S _ L) (a,b) ((S (x1,x2) R,(r2,r1,c):rs):ts) -- R on L
              | r2 == b && r2 == x2 = (a,b,c)
              | (a,b) == (r1,r2)    = (a,b,c >>= incr)
              | otherwise           = costAbove s (a,b) ((S (x1,x2) R,rs):ts)
costAbove s@(S _ L) (a,b) ((S (x1,x2) L,(r1,r2,c):rs):ts) -- L on L
              | r1 == b && r1 == x1 = minRange (a,b,c) (costAbove s (a,b) ((S (x1,x2) L,rs):ts))
              | (a,b) == (r1,r2)    = (a,b,c >>= incr)
              | otherwise           = costAbove s (a,b) ((S (x1,x2) L,rs):ts)
costAbove s@(S _ R) (a,b) ((S (x1,x2) L,(r1,r2,c):rs):ts) -- L on R
              | r1 == b && r1 == x1 = (a,b,c)
              | (b,a) == (r1,r2)    = (a,b,c >>= incr)
              | otherwise           = costAbove s (a,b) ((S (x1,x2) L,rs):ts)
costAbove s@(S _ N) (a,b) ((S (x1,x2) L,(r1,r2,c):rs):ts) -- L on N
              | r1 == b && r1 == x1 = minRange (a,b,c) (costAbove s (a,b) ((S (x1,x2) L,rs):ts))
              | r1 == a && r1 == x1 = (a,b,c)
              | (a,b) == (r1,r2)    = (a,b,c >>= incr)
              | otherwise           = costAbove s (a,b) ((S (x1,x2) L,rs):ts)
costAbove s@(S _ N) (a,b) ((S (x1,x2) R,(r2,r1,c):rs):ts) -- R on N
              | r2 == a && r2 == x2 = minRange (a,b,c) (costAbove s (a,b) ((S (x1,x2) R,rs):ts))
              | r2 == b && r2 == x2 = (a,b,c)
              | (a,b) == (r1,r2)    = (a,b,c >>= incr)
              | otherwise           = costAbove s (a,b) ((S (x1,x2) R,rs):ts)
costAbove s@(S _ _) (a,b) ((S (x1,x2) N,(r1,r2,c):rs):ts) -- N on any
              | (a,b) == (r1,r2)    = (a,b,c)
              | (b,a) == (r1,r2)    = (a,b,c)
              | otherwise           = costAbove s (a,b) ((S (x1,x2) N,rs):ts)


flow :: [WeightedRange] -> [WeightedRange]
flow []             = []
flow [w]            = [w]
flow ((r1,r2,c):ws) = (r1,r2,minCost c (thd3 $ head flowed)):flowed
                    where flowed = flow ws


weigh :: Range -> [(SimpleTarp,[Range])] -> WeightedTarps
weigh _ []                  = []
weigh v [(s,rs)]            = [(s,map (\(x,y) -> (x,y,Just 0)) rs)]
weigh v ((s@(S _ N),rs):ts) = (s,map (\r -> costAbove s r w) rs):w
                              where w = weigh v ts
weigh v ((s,rs):ts)         = (s,flow $ map (\r -> costAbove s r w) rs):w
                              where w = weigh v ts


-- solve by adding ground as extra tarp and get min Cost of all ranges
solution :: Input -> Int
solution (Input (a,b) _ ts)
         = minimum $ minimum <$> map thd3 $ flow vs
           where simp   = S (a,b) N:map simplify (reverse $ maxSort upper ts) ++ [S (a,b) N]
                 ranges = map head . group . sort $ intervals [a,b] simp
                 (s,vs) = head $ weigh (a,b) $ map (turn . (toRanges <$>) . sortOut . ( ,ranges)) simp
