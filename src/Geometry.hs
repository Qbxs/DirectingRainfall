module Geometry where

import Prelude hiding (length)
type Point = (Int,Int)

data Triangle = Tri Point Point Point

-- right-angled triangle: length of legs and triangle
data RTriangle = RTri Int Int Triangle

sqr :: (Num a) => a -> a
sqr x = x*x

-- length of a line
length :: Point -> Point -> Double
length (x1,y1) (x2,y2) = (sqrt . fromIntegral) $ (sqr $ x1-x2) + (sqr $ y1-y2)

-- area of triangle using heron's formula
area :: Triangle -> Double
area (Tri p1 p2 p3) = sqrt $ s * (s-a) * (s-b) * (s-c)
                      where a = length p1 p2
                            b = length p2 p3
                            c = length p3 p1
                            s = (a+b+c)/2

-- area of a right-angled triangle
-- assume right angle is at p2
areaRight :: RTriangle -> Double
areaRight (RTri a b _) = (fromIntegral $ a*b)/2

-- check whether a point is inside a right-angled triangle
pointInside :: Point -> RTriangle -> Bool
pointInside q t@(RTri _ _ (Tri p1 p2 p3)) = areaRight t == sum [area $ Tri q p1 p2,
                                                                area $ Tri q p1 p3,
                                                                area $ Tri q p2 p3]
