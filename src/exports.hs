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
instance ToJSON JInput where

inJ :: Input -> JInput
inJ (Input (a,b) _ ts) = JInput a b (map (\(T (x1,y1) (x2,y2)) -> [x1,y1,x2,y2]) ts)

writeInput :: JInput -> IO ()
writeInput s = do -- very inefficient
  encodeFile "/Users/pascalengel/Documents/acmContest/DirectingRainfall/js/temp.json" s
  i <- readFile "/Users/pascalengel/Documents/acmContest/DirectingRainfall/js/temp.json"
  writeFile "/Users/pascalengel/Documents/acmContest/DirectingRainfall/js/input.json" ("input="++i)
  removeFile "/Users/pascalengel/Documents/acmContest/DirectingRainfall/js/temp.json"
