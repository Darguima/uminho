-- 41. Considere que se usa o tipo [(a,Int)] para representar multi-conjuntos de elementos de a.
-- Considere ainda que nestas listas não há pares cuja primeira componente coincida, nem cuja
-- segunda componente seja menor ou igual a zero.
-- Defina a função insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)] que acrescenta
-- um elemento a um multi-conjunto.
-- Por exemplo, insereMSet 'c' [('b',2), ('a',4), ('c',1)] corresponde a [('b',2),
-- ('a',4), ('c',2)].

module Ex41 where

insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet newElement [] = [(newElement, 1)]
insereMSet newElement ((element, times) : t)
  | newElement == element = (element, times + 1) : t
  | otherwise = (element, times) : insereMSet newElement t
