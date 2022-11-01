-- 49. Considere os seguintes tipos para representar pontos e rectângulos, respectivamente. Assuma
-- que os rectângulos têm os lados paralelos aos eixos e são representados apenas por dois dos
-- pontos mais afastados.

-- type Ponto = (Float,Float)
-- data Rect = Rect Ponto Ponto

-- Defina a função totalArea :: [Rect] -> Float que, dada uma lista com rectângulos,
-- determina a área total que eles ocupam.

type Ponto = (Float,Float)
data Rect = Rect Ponto Ponto

totalArea :: [Rect] -> Float
totalArea [] = 0
totalArea (h : t) = rectArea h + totalArea t
  where rectArea :: Rect -> Float
        rectArea (Rect (x1, y1) (x2, y2)) = abs (x2-x1) * abs (y2-y1)
