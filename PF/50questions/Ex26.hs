-- 26. Apresente uma definição recursiva da função (pré-definida) nub :: Eq a => [a] -> [a] que
-- calcula uma lista com os mesmos elementos da recebida, sem repetições.
-- Por exemplo, nub [1,2,1,2,3,1,2] corresponde a [1,2,3].

nub :: Eq a => [a] -> [a]
nub [] = []
nub list = aux [] list
  where aux :: Eq a => [a] -> [a] -> [a]
        aux finalList [] = finalList
        aux finalList (h : t)
          | h `elem` finalList = aux finalList t
          | otherwise = aux (finalList ++ [h]) t
