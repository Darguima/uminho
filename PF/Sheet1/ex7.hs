data Point = Cartesian Double Double
  | Polar Double Double 
  deriving (Show,Eq)

data Figure = Circle Point Double
  | Rectangle Point Point
  | Triangle Point Point Point
  deriving (Show,Eq)

circ :: Figure
circ = Circle (Cartesian 0 0) 2.3

rect :: Figure
rect = Rectangle (Cartesian 0 0 ) (Cartesian (-2) (2))

triangle :: Figure
triangle = Triangle (Cartesian 0 0) (Cartesian 2 2) (Cartesian 4 4)

polygon :: Figure -> Bool
polygon (Circle _ _) = False
polygon (Rectangle _ _) = True
polygon (Triangle _ _ _ ) = True
-- polygon _ = False -> Could use this in the two up lines (with true)

vertices :: Figure -> [Point]
vertices (Circle _ _) = []
vertices (Triangle a b c) = [a, b, c]
vertices (Rectangle (Cartesian x1 y1) (Cartesian x2 y2)) = [Cartesian x1 y1, Cartesian x2 y2, Cartesian x1 y2, Cartesian x2 y1]

area :: Figure -> Double
area (Circle _ radius) = 3.1416 * radius^2
area (Rectangle (Cartesian x1 y1) (Cartesian x2 y2)) = abs (x2 - x1) * (y2 - y1)
area (Triangle (Cartesian x1 y1) (Cartesian x2 y2) (Cartesian x3 y3)) = 27 --  formula de Heron
