module Tarps where

import Data.List
import Data.Tuple
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
    Just (x,y) -> if a2 >= d2 then True else
                  if c2 >= b2 then False else
               -- here: all cases for which t1 overlaps t2 partially
               -- consider all combinations of {a,b} x {c,d} => 8
                     x == a1 && y == c1 && a2 > c2
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
maxSort p l  = (fst $ maxList l):maxSort p (snd $ maxList l)
         where maxList [x]    = (x,[])
               maxList (x:xs) = if p x (fst $ maxList xs)
                                then (x,(fst $ maxList xs):(snd $ maxList xs))
                                else (fst $ maxList xs,x:(snd $ maxList xs))



-- simplified tarp after sort without y-coordinates: (x1,x2)
-- if x1<x2 then tarp points to the left else to the right
data SimpleTarp = S Range Orientation
  deriving (Show, Eq)
data Orientation = L | R -- TODO: add neutral as vineyard
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
clean :: [(a,[Int])] -> [(a,[Int])]
clean = map $ fmap $ map head . group . sort


-- METHOD B: get all intervals for computation efficency

-- gives list of all relevant intervals, first arg is range as a 2-elem-list
intervals :: [Int] -> [SimpleTarp] -> [Int]
intervals = foldr $ \(S (x1,x2) _) xs -> [x1,x2] ++ xs

-- sort out intervals that are not part of the tarp
sortOut :: (SimpleTarp,[Int]) -> (SimpleTarp,[Int])
sortOut ((S (x1,x2) o),[])   = ((S (x1,x2) o),[])
sortOut ((S (x1,x2) o),r:rs)
                 | r < x1    = sortOut (S (x1,x2) o,rs)
                 | r > x2    = (S (x1,x2) o,[])
                 | otherwise = (\(x,ys) -> (x,r:ys)) $ sortOut (S (x1,x2) o,rs)

toRanges :: [Int] -> [Range]
toRanges [] = []
toRanges [_] = []
toRanges (a:xs@(b:_)) = (a,b):(toRanges xs)

-- turn around intervals if tarp is orienteted towards left
turn :: (SimpleTarp,[Range]) -> (SimpleTarp,[Range])
turn s@((S _ R),_) = (reverse . map swap) <$> s
turn s             = s

-- new type for tarps, split into intervals with cost to reach
type WeightedTarps = [(SimpleTarp,[WeightedRange])]
type WeightedRange = (Int,Int,Cost)
type Cost          = Maybe Int


incr :: Num a => a -> Maybe a
incr x = Just $ x + 1

-- calculate costs of an interval of a tarp based on costs of tarps above
costAbove :: SimpleTarp -> Range -> WeightedTarps -> WeightedRange
costAbove _ (a,b)       []          = (a,b,Nothing)
costAbove s r           ((_,[]):ts) = costAbove s r ts
costAbove s@(S _ R) (a,b) ((S (x1,x2) R,(r2,r1,c):rs):ts)
              | r2 == b && r2 == x2 = (a,b,c)
              | (b,a) == (r1,r2)    = (a,b,c >>= incr)
              | otherwise           = costAbove s (a,b) ((S (x1,x2) R,rs):ts)
costAbove s@(S _ L) (a,b) ((S (x1,x2) R,(r2,r1,c):rs):ts)
              | r2 == b && r2 == x2 = (a,b,c)
              | (a,b) == (r1,r2)    = (a,b,c >>= incr)
              | otherwise           = costAbove s (a,b) ((S (x1,x2) R,rs):ts)
costAbove s@(S _ L) (a,b) ((S (x1,x2) L,(r1,r2,c):rs):ts)
              | r1 == b && r1 == x1 = (a,b,c)
              | (a,b) == (r1,r2)    = (a,b,c >>= incr)
              | otherwise           = costAbove s (a,b) ((S (x1,x2) L,rs):ts)
costAbove s@(S _ R) (a,b) ((S (x1,x2) L,(r1,r2,c):rs):ts)
              | r1 == b && r1 == x1 = (a,b,c)
              | (b,a) == (r1,r2)    = (a,b,c >>= incr)
              | otherwise           = costAbove s (a,b) ((S (x1,x2) L,rs):ts)


minCost :: Cost -> Cost -> Cost
minCost (Just c) (Just d) = Just $ min c d
minCost (Just c) Nothing  = Just c
minCost Nothing  (Just d) = Just d
minCost Nothing  Nothing  = Nothing


flow :: [WeightedRange] -> [WeightedRange]
flow []             = []
flow [w]            = [w]
flow ((r1,r2,c):ws) = (r1,r2,minCost c (thd3 $ head flowed)):flowed
                    where flowed = flow ws


-- calculate costs for upmost tarp (condition: has to be reachable) TODO unreachable cases
initialise :: Range -> (SimpleTarp,[Range]) -> (SimpleTarp,[WeightedRange])
initialise _     (s,[]) = (s,[])
initialise (a,b) (S t R,(r1,r2):rs)
              | r2 >= a && r1 <= b = (\(x,ys) -> (x,(r1,r2,Just 0):ys)) $ initialise (a,b) (S t R,rs)
              | otherwise          = (\(x,ys) -> (x,(r1,r2,Nothing):ys)) $ initialise (a,b) (S t R,rs)
initialise (a,b) (S t L,(r1,r2):rs)
              | r1 >= a && r2 <= b = (\(x,ys) -> (x,(r1,r2,Just 0):ys)) $ initialise (a,b) (S t L,rs)
              | otherwise          = (\(x,ys) -> (x,(r1,r2,Nothing):ys)) $ initialise (a,b) (S t L,rs)


weigh :: Range -> [(SimpleTarp,[Range])] -> WeightedTarps
weigh _ []          = []
weigh v [s]         = [flow <$> initialise v s]
weigh v ((s,rs):ts) = (s,flow $ map (\r -> costAbove s r w) rs):w
                        where w = weigh v ts


-- we don't want "Just" in our output
unJust :: (Show a) => Maybe a -> String
unJust Nothing = "Nothing"
unJust (Just x) = show x

-- solve by adding ground as extra tarp and get min Cost of all ranges
solution :: Input -> String
solution (Input (a,b) _ ts) = (unJust . thd3 . head) vs
                          where simp = (S (a,b) R):(map simplify $ reverse $ maxSort upper ts)
                                ivs = map head . group . sort $ intervals [a,b] simp
                                (s,vs) = head $ weigh (a,b) $ map turn $ map (toRanges <$>) $ map sortOut $ map (\x -> (x,ivs)) simp
