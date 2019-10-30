import Data.List

-- point with x,y coordinates
data Point = P Int Int
  deriving(Show,Eq)

-- tarp consisting of a line connecting two points
data Tarp = T Point Point
  deriving(Show,Eq)

-- complete input:
-- Interval (a,b)
-- N° of tarps: n
-- List of tarps
data Input = I (Int,Int) Int [Tarp]
  deriving(Show,Eq,Read)
