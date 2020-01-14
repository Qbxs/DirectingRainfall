module Geometry where

import Prelude hiding (length)
type Point = (Int,Int)

data Triangle = Tri Point Point Point
   deriving(Show,Eq)

sqr :: (Num a) => a -> a
sqr x = x*x

length :: Point -> Point -> Double
length (x1,y1) (x2,y2) = (sqrt . fromIntegral) $ (sqr $ x1-x2) + (sqr $ y1-y2)

-- triangle area using heron's formula
area :: Triangle -> Double
area (Tri p1 p2 p3) = sqrt $ s * (s-a) * (s-b) * (s-c)
                      where a = length p1 p2
                            b = length p2 p3
                            c = length p3 p1
                            s = (a+b+c)/2

-- check whether a point is inside a triangle
pointInside :: Point -> Triangle -> Bool
pointInside q (Tri p1 p2 p3) = area (Tri p1 p2 p3) == sum [area $ Tri q p1 p2,
                                                           area $ Tri q p1 p3,
                                                           area $ Tri q p2 p3]
