{-# LANGUAGE TupleSections #-}

module Tarps where

import Data.List
import Data.Tuple
import Data.Maybe
import Control.Applicative
import InputParser (Input(..), Range, Point, Tarp(..))
import Geometry

instance Functor ((,,) a b) where
  fmap f (x,y,z) = (x,y,f z)

thd3 :: (a,b,c) -> c
thd3 = \(a,b,c) -> c

-- Check whether two ranges overlap and return overlapping range
overlap :: Range -> Range -> Maybe Range
overlap (a,b) (c,d) | c > b || a > d = Nothing
                    | otherwise      = Just (max a c,min b d)

tarpRange :: Tarp -> Range
tarpRange (T (a,_) (b,_)) = (min a b,max a b)


upper :: Tarp -> Tarp -> Maybe Bool
upper t1@(T a@(a1,a2) b@(b1,b2)) t2@(T c@(c1,c2) d@(d1,d2))
  = case overlap (tarpRange t1) (tarpRange t2) of
    Nothing    -> Nothing
    Just (x,y) -> Just $ a2 >= d2 ||
                         (c2 < b2
                          && (x == a1 && pointAbove a (toLine c d)
                           || y == a1 && pointAbove a (toLine c d)
                           || x == b1 && pointAbove b (toLine c d)
                           || y == b1 && pointAbove b (toLine c d)
                           || x == c1 && (not $ pointAbove d (toLine a b))
                           || y == c1 && (not $ pointAbove d (toLine a b))))


-- quadratic but safe sorting (upper is not transitive)
maxSort :: Eq a => (a -> a -> Maybe Bool) -> [a] -> [a]
maxSort p [] = []
maxSort p ts  = maxSort p (ts\\m) ++ m
              where m = [t | t<-ts, all (fromMaybe True . p t) $ ts\\[t]]

-- simplified tarp after sort without y-coordinates: (x1,x2)
-- if x1<x2, then tarp points to the left, else to the right
data SimpleTarp = S Range Orientation
  deriving (Show, Eq)
data Orientation = L | R | N --left | right | neutral(vineyard)
  deriving (Show,Eq)

simplify :: Tarp -> SimpleTarp
simplify t@(T (x1,_) (x2,_)) = S (tarpRange t) $ if x1 < x2 then L else R

-- get all intervals once for computation efficency
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

-- turn around intervals if tarp is orienteted towards right
turn :: (SimpleTarp,[Range]) -> (SimpleTarp,[Range])
turn s@(S _ R,_) = reverse . map swap <$> s
turn s           = s

-- new type for tarps, split into ranges with cost to reach
type WeightedTarps = [(SimpleTarp,[WeightedRange])]
type WeightedRange = (Int,Int,Cost)
type Cost          = Maybe Int

-- minCost unlike (min <$>)
minCost :: Cost -> Cost -> Cost
minCost (Just c) (Just d) = Just $ min c d
minCost (Just c) Nothing  = Just c
minCost Nothing  (Just d) = Just d
minCost Nothing  Nothing  = Nothing

minRange :: WeightedRange -> WeightedRange -> WeightedRange
minRange (a,b,c) (_,_,z) = (a,b,minCost c z)

incr :: (Num a) => a -> Maybe a
incr = Just . (1+)

-- calculate costs of an interval of a tarp based on costs of tarps above
costAbove :: SimpleTarp -> Range -> WeightedTarps -> WeightedRange
costAbove _         (a,b) []        = (a,b,Nothing)
costAbove s         r   ((_,[]):ts) = costAbove s r ts
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

-- let water flow down a tarp (overwriting with minimum)
flow :: [WeightedRange] -> [WeightedRange]
flow []             = []
flow [w]            = [w]
flow ((r1,r2,c):ws) = (r1,r2,minCost c (thd3 $ head flowed)):flowed
                    where flowed = flow ws

-- filter out tarps that don't overlap with given tarp
filterTarps :: SimpleTarp -> WeightedTarps -> WeightedTarps
filterTarps s = filter (tarpOverlap s) where
  tarpOverlap (S a _) (S b _,_) = isJust $ overlap a b

-- calculate all costs
weigh :: Range -> [(SimpleTarp,[Range])] -> WeightedTarps
weigh _ []                  = []
weigh v [(s,rs)]            = [(s,map (\(x,y) -> (x,y,Just 0)) rs)]
weigh v ((s@(S _ N),rs):ts) = (s,map (\r -> costAbove s r (filterTarps s w)) rs):w
                              where w = weigh v ts
weigh v ((s,rs):ts)         = (s,flow $ map (\r -> costAbove s r (filterTarps s w)) rs):w
                              where w = weigh v ts


-- solve by adding ground as extra tarp and get min Cost of all ranges
solution :: Input -> Int
solution (Input (a,b) _ ts) = fromJust $ minimum <$> map thd3 $ flow rs
           where simp   = S (a,b) N:map simplify (maxSort upper ts) ++ [S (a,b) N]
                 ranges = map head . group . sort $ intervals [a,b] simp
                 (s,rs) = head $ weigh (a,b) $ map (turn . (toRanges <$>) . sortOut . ( ,ranges)) simp
