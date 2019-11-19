{-# LANGUAGE DeriveGeneric #-}

module Exports where

import InputParser (Input(..), Range, Point, Tarp(..))
import Tarps
import Data.Aeson
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
writeInput s = do -- very inefficient
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
siJ (Input (a,b) _ ts) = JSimple a b (map (\(S (x,y) o) -> [x,y,if o == L then 0 else 1]) (map simplify (reverse (maxSort upper ts))))

writeSimple :: JSimple -> IO ()
writeSimple s = do -- very inefficient
  encodeFile "/Users/pascalengel/Documents/acmContest/DirectingRainfall/js/temp.json" s
  i <- readFile "/Users/pascalengel/Documents/acmContest/DirectingRainfall/js/temp.json"
  writeFile "/Users/pascalengel/Documents/acmContest/DirectingRainfall/js/simple.json" ("simple="++i)
  removeFile "/Users/pascalengel/Documents/acmContest/DirectingRainfall/js/temp.json"
