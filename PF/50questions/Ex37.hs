-- 37. Apresente uma definição recursiva da função iSort :: Ord a => [a] -> [a] que calcula
-- o resultado de ordenar uma lista. Assuma, se precisar, que existe definida a função 
-- insert :: Ord a => a -> [a] -> [a] que dado um elemento e uma lista ordenada retorna a lista
-- resultante de inserir ordenadamente esse elemento na lista.

import Ex31 (insert)  

iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort l = aux [] l 
  where aux :: Ord a => [a] -> [a] -> [a]
        aux returnList [] = returnList
        aux returnList (h : t) = aux (insert h returnList) t
