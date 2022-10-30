-- 39. Considere que se usa o tipo [(a,Int)] para representar multi-conjuntos de elementos de a.
-- Considere ainda que nestas listas não há pares cuja primeira componente coincida, nem cuja
-- segunda componente seja menor ou igual a zero.
-- Defina a função elemMSet :: Eq a => a -> [(a,Int)] -> Bool que testa se um elemento
-- pertence a um multi-conjunto.
-- Por exemplo, elemMSet 'a' [('b',2), ('a',4), ('c',1)] corresponde a True enquanto
-- que elemMSet 'd' [('b',2), ('a',4), ('c',1)] corresponde a False.

elemMSet :: Eq a => a -> [(a,Int)] -> Bool
elemMSet _ [] = False
elemMSet searchElement ((a, _) : t)
  | searchElement == a = True
  | otherwise = elemMSet searchElement t