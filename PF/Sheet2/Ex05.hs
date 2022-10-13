-- sumNegative :: [Int] -> Int que soma todos os números negativos da lista de entrada.

sumNegative :: [Int] -> Int
sumNegative [] = 0
sumNegative (h : t)
  | h < 0 = h + sumNegative t
  | otherwise = sumNegative t
