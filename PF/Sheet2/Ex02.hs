--  numOcorre :: Char -> String -> Int que calcula o número de vezes que um
-- caracter ocorre numa string.

numOccurs :: Char -> String -> Int
numOccurs _ [] = 0
numOccurs char (h : t)
  | char == h = 1 + numOccurs char t
  | otherwise = numOccurs char t
