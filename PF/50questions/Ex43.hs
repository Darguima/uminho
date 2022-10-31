-- 43. Considere que se usa o tipo [(a,Int)] para representar multi-conjuntos de elementos de a.
-- Considere ainda que nestas listas não há pares cuja primeira componente coincida, nem cuja
-- segunda componente seja menor ou igual a zero.
-- Defina a função constroiMSet :: Ord a => [a] -> [(a,Int)] dada uma lista ordenada
-- por ordem crescente, calcula o multi-conjunto dos seus elementos.
-- Por exemplo, constroiMSet "aaabccc" corresponde a [('a',3), ('b',1), ('c',3)].

import Ex41 (insereMSet)

constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet l = aux l []
  where aux :: Ord a => [a] -> [(a,Int)] -> [(a,Int)]
        aux [] returnList = returnList
        aux (h : t) returnList = aux t (insereMSet h returnList)
