-- 8. Apresente uma definição recursiva da função (pré-definida) zip :: [a] -> [b] -> [(a,b)]
-- constói uma lista de pares a partir de duas listas.
-- Por exemplo, zip [1,2,3] [10,20,30,40] corresponde a [(1,10),(2,20),(3,30)].

myZip :: [a] -> [b] -> [(a,b)]
myZip (x : xs) (y : ys) = (x, y) : myZip xs ys
myZip _ _ = []
