-- 40. Considere que se usa o tipo [(a,Int)] para representar multi-conjuntos de elementos de a.
-- Considere ainda que nestas listas não há pares cuja primeira componente coincida, nem cuja
-- segunda componente seja menor ou igual a zero.
-- Defina a função converteMSet ::
--  [(a,Int)] -> [a] que converte um multi-conjuto na
-- lista dos seus elementos
-- Por exemplo, converteMSet [('b',2), ('a',4), ('c',1)] corresponde a "bbaaaac".

converteMSet :: [(a,Int)] -> [a]
converteMSet [] = []
converteMSet ((element, repetitions) : t)
  | repetitions <= 0 = converteMSet t
  | otherwise = element : converteMSet ((element, repetitions - 1) : t)
