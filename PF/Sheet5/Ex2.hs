type Polinomio = [Monomio]
type Monomio = (Float,Int)

-- a) selGrau :: Int -> Polinomio -> Polinomio que selecciona os monómios com
-- um dado grau de um polinómio.

selGrau :: Int -> Polinomio -> Polinomio
selGrau grau = filter (\x -> snd x == grau)

-- b) conta :: Int -> Polinomio -> Int de forma a que (conta n p) indica quan-
-- tos monómios de grau n existem em p.

conta :: Int -> Polinomio -> Int
conta grau pol = length (selGrau grau pol)

-- e) calcula :: Float -> Polinomio -> Float que calcula o valor de um polinómio para uma dado valor de x.
calcula :: Float -> Polinomio -> Float
calcula x ((coeficiente, expoente) : t) = sum (map () )
