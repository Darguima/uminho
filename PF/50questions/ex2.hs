-- 2. Apresente uma definição recursiva da função (pré-definida) myEnumFromThenTo :: Int -> Int
-- -> Int -> [Int] que constrói a lista dos números inteiros compreendidos entre dois limites
-- e espaçados de um valor constante.
-- Por exemplo, myEnumFromThenTo 1 3 10 corresponde à lista [1,3,5,7,9].

myEnumFromThenTo :: Int -> Int -> Int -> [Int]
myEnumFromThenTo start step end
  | start > end = []
  | start == end = [start]
  | otherwise = start : myEnumFromThenTo (start + step) step end