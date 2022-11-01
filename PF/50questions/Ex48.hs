-- 48. Considere os seguintes tipos para representar pontos e rectângulos, respectivamente. Assuma
-- que os rectângulos têm os lados paralelos aos eixos e são representados apenas por dois dos
-- pontos mais afastados.

-- type Ponto = (Float,Float)
-- data Rect = Rect Ponto Ponto

-- Defina a função countSquares :: [Rect] -> Int que, dada uma lista com rectângulos,
-- conta quantos deles são quadrados.

type Ponto = (Float,Float)
data Rect = Rect Ponto Ponto

countSquares :: [Rect] -> Int
countSquares [] = 0
countSquares ((Rect (x1, y1) (x2, y2)) : t)
  | abs (x2-x1) == abs (y2-y1) = 1 + countSquares t
  | otherwise = countSquares t
