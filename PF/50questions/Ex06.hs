-- 6. Apresente uma definição recursiva da função (pré-definida) take :: Int -> [a] -> [a] que
-- dado um inteiro n e uma lista l calcula a lista com os (no máximo) n primeiros elementos de l.
-- A lista resultado só terá menos de que n elementos se a lista l tiver menos do que n elementos.
-- Nesse caso a lista calculada é igual à lista fornecida.
-- Por exemplo, take 2 [10,20,30] corresponde a [10,20].

myTake :: Int -> [a] -> [a]
myTake n [] = []
myTake 0 _ = []
myTake n (h : t) = h : myTake (n-1) t
