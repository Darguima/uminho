import Ex1 (sortOn_)
import Data.List (groupBy)

type Polinomio = [Monomio]
type Monomio = (Float,Int)

-- (a) selgrau :: Int -> Polinomio -> Polinomio que selecciona os monómios com um dado grau de um polinómio.
selgrau :: Int -> Polinomio -> Polinomio
selgrau grau = filter (\(_, exp) -> exp == grau)

-- (b) conta :: Int -> Polinomio -> Int de forma a que (conta n p) indica quantos monómios de grau n existem em p.
conta :: Int -> Polinomio -> Int
conta grau pol = length (selgrau grau pol)

-- (c) grau :: Polinomio -> Int que indica o grau de um polinómio.
grau :: Polinomio -> Int
grau = foldr (\(_, grau) x -> max x grau) 0

-- (d) deriv :: Polinomio -> Polinomio que calcula a derivada de um polinómio.
deriv :: Polinomio -> Polinomio
deriv = map derivMon
  where derivMon :: Monomio -> Monomio
        derivMon (coeficiente, expoente)
          | expoente > 0 = (coeficiente * fromIntegral expoente, expoente - 1)
          | otherwise = (0, 0)

-- (e) calcula :: Float -> Polinomio -> Float que calcula o valor de um polinómio para uma dado valor de x.
calcula :: Float -> Polinomio -> Float
calcula x pol = sum ( map (\(coeficiente, expoente) -> coeficiente * (x ^ expoente)) pol )

-- (f) simp :: Polinomio -> Polinomio que retira de um polinómio os monómios de coeficiente zero.
simp :: Polinomio -> Polinomio
simp = filter (\(coeficiente, _) -> coeficiente /= 0)

-- (g) mult :: Monomio -> Polinomio -> Polinomio que calcula o resultado da multiplicação de um monómio por um polinómio.
mult :: Monomio -> Polinomio -> Polinomio
mult (mCoeficiente, mExpoente) = map (\(coeficiente, expoente) -> (coeficiente * mCoeficiente, expoente + mExpoente))

-- (h) ordena :: Polinomio -> Polinomio que ordena um polinómio por ordem crescente dos graus dos seus monómios.
ordena :: Polinomio -> Polinomio
ordena = sortOn_ snd

-- (i) normaliza :: Polinomio -> Polinomio que dado um polinómio constrói um polinómio equivalente em que não podem aparecer varios monómios com o mesmo grau.
normaliza :: Polinomio -> Polinomio
normaliza pol = map (foldr (\ (accCoef, grau) (currentCoef, _) -> (accCoef + currentCoef, grau)) (0, 0)) $ groupBy (\m1 m2 -> snd m1 == snd m2) $ ordena pol

-- (j) soma :: Polinomio -> Polinomio -> Polinomio que faz a soma de dois polinómios de forma que se os polinómios que recebe estiverem normalizados produz também um polinómio normalizado.
soma :: Polinomio -> Polinomio -> Polinomio
soma p1 p2 = normaliza $ p1 ++ p2

-- (k) produto :: Polinomio -> Polinomio -> Polinomio que calcula o produto de dois polinómios
produto :: Polinomio -> Polinomio -> Polinomio
produto pol1 pol2 = foldr ((++) . (`mult` pol2)) [] pol1
 
-- (l) equiv :: Polinomio -> Polinomio -> Bool que testa se dois polinómios são equivalentes.
-- equiv :: Polinomio -> Polinomio -> Bool
