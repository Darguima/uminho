-- Apresente uma definição recursiva da função (pré-definida) intersperse :: a -> [a] -> [a]
-- que dado um elemento e uma lista, constrói uma lista em que o elemento fornecido é
-- intercalado entre os elementos da lista fornecida.
-- Por exemplo, intersperce 1 [10,20,30] corresponde a [10,1,20,1,30].

myIntersperse :: a -> [a] -> [a]
myIntersperse n (h : t)
  | null t = [h]
  | otherwise = h : n : myIntersperse n t
