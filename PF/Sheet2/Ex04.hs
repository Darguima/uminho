--  onlyPositive :: [Int] -> [Int] que retira todos os elementos não positivos de uma lista de inteiros.

onlyPositive :: [Int] -> [Int]
onlyPositive [] = []
onlyPositive (h : t)
  | h >= 0 = h : onlyPositive t
  | otherwise = onlyPositive t
