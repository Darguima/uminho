-- 29. Apresente uma definição recursiva da função (pré-definida) union :: Eq a => [a] -> [a] -> [a]
-- que retorna a lista resultante de acrescentar à primeira lista os elementos da segunda
-- que não ocorrem na primeira.
-- Por exemplo, union [1,1,2,3,4] [1,5] corresponde a [1,1,2,3,4,5].

union :: Eq a => [a] -> [a] -> [a]
union l [] = l
union finalList (h : t)
  | h `elem` finalList = union finalList t
  | otherwise = union (finalList ++ [h]) t
