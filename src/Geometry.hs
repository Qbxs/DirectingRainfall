module Geometry where

type Point = (Int,Int)

-- Line y = m*x+b where m and b are given
data Line = Line Double Double
instance Show Line where
  show (Line m b) = "y="++show m++"*x+"++show b

-- get line equation from two points
toLine :: Point -> Point -> Line
toLine (x1,y1) (x2,y2) = let m = ((fromIntegral y2)-(fromIntegral y1))/((fromIntegral x2) - (fromIntegral x1)) :: Double
                             b = -m*(fromIntegral x1) + (fromIntegral y1) :: Double
                             in Line m b

-- calculate y-value for given x-value on line
yLine :: Double -> Line -> Double
yLine x (Line m b) = m*x+b

-- is a point above the line?
pointAbove :: Point -> Line -> Bool
pointAbove (p1,p2) g = (yLine (fromIntegral p1) g) - (fromIntegral p2) < 0
