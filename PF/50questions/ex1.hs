-- 1. Apresente uma definição recursiva da função (pré-definida) myEnumFromTo :: Int -> Int ->
-- [Int] que constrói a lista dos números inteiros compreendidos entre dois limites.
-- Por exemplo, myEnumFromTo 1 5 corresponde à lista [1,2,3,4,5]

myEnumFromTo :: Int -> Int -> [Int]
myEnumFromTo start end
  | start > end = []
  | start == end = [start]
  | otherwise = start : myEnumFromTo (1 + start) end
