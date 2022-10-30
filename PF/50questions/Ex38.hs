-- 38. Apresente uma definição recursiva da função menor :: String -> String -> Bool que
-- dadas duas strings, retorna True se e só se a primeira for menor do que a segunda, segundo
-- a ordem lexicográfica (i.e., do dicionário)
-- Por exemplo, menor "sai" "saiu" corresponde a True enquanto que menor "programacao"
-- "funcional" corresponde a False.

import Data.Char (ord, toLower)

menor :: String -> String -> Bool
menor "" _ = True
menor _ "" = False
menor (h1 : t1) (h2 : t2)
  | h1Int < h2Int = True
  | h2Int < h1Int = False
  | otherwise = menor t1 t2
  where (h1Int, h2Int) = (ord (toLower h1), ord (toLower h2))
