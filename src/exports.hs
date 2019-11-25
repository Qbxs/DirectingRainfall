{-# LANGUAGE DeriveGeneric #-}

module Exports where

import InputParser (Input(..), Range, Point, Tarp(..))
import Tarps
import Data.Aeson
import Data.List
import GHC.Generics
import System.Directory


data JInput = JInput {
      a     :: Int,
      b     :: Int,
      tarps :: [[Int]]
      } deriving (Show,Generic)
instance ToJSON JInput

inJ :: Input -> JInput
inJ (Input (a,b) _ ts) = JInput a b (map (\(T (x1,y1) (x2,y2)) -> [x1,y1,x2,y2]) ts)

writeInput :: JInput -> IO ()
writeInput s = do
  encodeFile "/Users/pascalengel/Documents/acmContest/DirectingRainfall/js/temp.json" s
  i <- readFile "/Users/pascalengel/Documents/acmContest/DirectingRainfall/js/temp.json"
  writeFile "/Users/pascalengel/Documents/acmContest/DirectingRainfall/js/input.json" ("input="++i)
  removeFile "/Users/pascalengel/Documents/acmContest/DirectingRainfall/js/temp.json"


data JSimple = JSimple {
      sa       :: Int,
      sb       :: Int,
      starps   :: [[Int]]
      } deriving (Show,Generic)
instance ToJSON JSimple

siJ :: Input -> JSimple
siJ (Input (a,b) _ ts) = JSimple a b
                          (map (\(S (x,y) o) -> [x,y,if o == L then 0 else 1])
                           (map simplify (reverse (maxSort upper ts))))

writeSimple :: JSimple -> IO ()
writeSimple s = do
  encodeFile "/Users/pascalengel/Documents/acmContest/DirectingRainfall/js/temp.json" s
  i <- readFile "/Users/pascalengel/Documents/acmContest/DirectingRainfall/js/temp.json"
  writeFile "/Users/pascalengel/Documents/acmContest/DirectingRainfall/js/simple.json" ("simple="++i)
  removeFile "/Users/pascalengel/Documents/acmContest/DirectingRainfall/js/temp.json"

data JTarp = JTarp {
      x1    :: Int,
      x2    :: Int,
      o     :: Int,
      costs :: [[Int]]
      } deriving (Show,Generic)
instance ToJSON JTarp

data JWeighted = JWeighted {
      wa     :: Int,
      wb     :: Int,
      wtarps :: [JTarp]
      } deriving (Show,Generic)
instance ToJSON JWeighted

weighted :: Input -> WeightedTarps
weighted (Input (a,b) _ ts) = weigh (a,b) $ map turn $ map (toRanges <$>)
                              $ map sortOut $ map (\x -> (x,ivs)) simp
                          where simp = (S (a,b) N):(map simplify $ reverse $ maxSort upper ts)++[S (a,b) N]
                                ivs = map head . group . sort $ intervals [a,b] simp

fromMaybe :: Cost -> Int
fromMaybe Nothing  = -1
fromMaybe (Just x) = x

weJ :: Input -> JWeighted
weJ i@(Input (a,b) _ ts) = JWeighted a b $
                                (map (\(S (x1,x2) o,rs)
                                  -> JTarp x1 x2 (if o == L then -1 else if o == R then 1 else 0)
                                    (map (\(x,y,c) -> [x,y,fromMaybe c]) rs)) $ weighted i)

writeWeighted :: JWeighted -> IO ()
writeWeighted s = do
  encodeFile "/Users/pascalengel/Documents/acmContest/DirectingRainfall/js/temp.json" s
  i <- readFile "/Users/pascalengel/Documents/acmContest/DirectingRainfall/js/temp.json"
  writeFile "/Users/pascalengel/Documents/acmContest/DirectingRainfall/js/weighted.json" ("weighted="++i)
  removeFile "/Users/pascalengel/Documents/acmContest/DirectingRainfall/js/temp.json"
