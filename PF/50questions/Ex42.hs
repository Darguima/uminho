-- 42. Considere que se usa o tipo [(a,Int)] para representar multi-conjuntos de elementos de a.
-- Considere ainda que nestas listas não há pares cuja primeira componente coincida, nem cuja
-- segunda componente seja menor ou igual a zero.
-- Defina a função removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)] que remove um
-- elemento a um multi-conjunto. Se o elemento não existir, deve ser retornado o multi-conjunto
-- recebido.
-- Por exemplo, removeMSet 'c' [('b',2), ('a',4), ('c',1)] corresponde a [('b',2),
-- ('a',4)].

removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet searchElement [] = []
removeMSet searchElement ((element, times) : t)
  | searchElement == element && times <= 1 = t
  | searchElement == element = (element, times - 1) : t
  | otherwise = (element, times) : removeMSet searchElement t
