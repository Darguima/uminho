data Point = Cartesian Double Double | Polar Double Double
  deriving (Show, Eq)

posX :: Point -> Double
posX (Cartesian x _) = x

posY :: Point -> Double
posY (Cartesian _ y) = y

radius :: Point -> Double
radius (Cartesian x y) = sqrt (x^2 + y^2)

angle :: Point
angle (Cartesian x y) = atan y/x
