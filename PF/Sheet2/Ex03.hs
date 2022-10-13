-- positivos :: [Int] -> Bool que testa se uma lista só tem elementos positivos.

positives :: [Int] -> Bool
positives [] = True
positives (h : t)
  | h >= 0 = positives t
  | otherwise = False

