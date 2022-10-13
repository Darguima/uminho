-- 4. Uma forma de representar polinómios de uma variável é usar listas de monómios rep- resentados por pares (coeficiente, expoente)

-- type Polinomio = [Monomio]
-- type Monomio = (Float,Int)

-- Por exemplo, [(2,3), (3,4), (5,3), (4,5)] representa o polinómio 2 x3 + 3 x4 +
-- 5 x3 + 4 x5 . Defina as seguintes funções:

-- (a) count :: Int -> Polynomial -> Int de forma a que (conta n p) indica quantos monómios de grau n existem em p.
-- (b) polynomialGrade :: Polynomial -> Int que indica o grau de um polinómio.
-- (c) selectGrade :: Int -> Polynomial -> Polynomial que selecciona os monómios com um dado grau de um polinómio.
-- (d) derivate :: Polynomial -> Polynomial que calcula a derivada de um polinómio.
-- (e) calcula :: Float -> Polynomial -> Float que calcula o valor de um polinómio para uma dado valor de x.
-- (f) simp :: Polynomial -> Polynomial que retira de um polinómio os monómios de coeficiente zero.
-- (g) mult :: Monomio -> Polynomial -> Polynomial que calcula o resultado da mul- tiplicação de um monómio por um polinómio.
-- (h) normaliza :: Polynomial -> Polynomial que dado um polinómio constrói um polinómio equivalente em que não podem aparecer varios monómios com o mesmo grau.
-- (i) soma :: Polynomial -> Polynomial -> Polynomial que soma dois polinómios de forma a que se os polinómios que recebe estiverem normalizados produz também um polinómio normalizado.
-- (j) produto :: Polynomial -> Polynomial -> Polynomial que calcula o produto de dois polinómios
-- (k) ordena :: Polynomial -> Polynomial que ordena um polinómio por ordem cres- cente dos graus dos seus monómios.
-- (l) equiv :: Polynomial -> Polynomial -> Bool que testa se dois polinómios são equivalentes.

type Monomial = (Float, Int)
type Polynomial = [Monomial]

count :: Int -> Polynomial -> Int
count _ [] = 0
count searchGrade ((_, monomialGrade) : t)
  | searchGrade == monomialGrade = 1 + count searchGrade t
  | otherwise = count searchGrade t

polynomialGrade :: Polynomial -> Int
polynomialGrade [] = 0
polynomialGrade l = searchHighestGrade 0 l 
  where searchHighestGrade :: Int -> Polynomial -> Int
        searchHighestGrade currentGrade [] = currentGrade
        searchHighestGrade currentGrade ((_, monomialGrade) : t)
          | currentGrade > monomialGrade = searchHighestGrade currentGrade t
          | otherwise = searchHighestGrade monomialGrade t

polynomialGrade2 :: Polynomial -> Int
polynomialGrade2 [] = 0
polynomialGrade2 [(_, grade)] = grade
polynomialGrade2 ((c1, grade1) : (c2, grade2) : t)
  | grade1 > grade2 = polynomialGrade ((c1, grade1) : t)
  | otherwise = polynomialGrade ((c2, grade2) : t)

monomialGrade :: Monomial -> Int
monomialGrade (c, g) = g

polynomialGrade3 :: Polynomial -> Int
polynomialGrade3 [m] = monomialGrade m
polynomialGrade3 (m : ms) = let g = polynomialGrade3 ms
                            in max (monomialGrade m) g  

-- selectGrade :: Int -> Polynomial -> Polynomial

derivate :: Polynomial -> Polynomial
derivate [] = []
derivate ((coefficient, grade) : t)
  | grade == 0 = derivate t
  | otherwise = (coefficient, grade - 1) : derivate t
