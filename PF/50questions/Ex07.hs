-- 7. Apresente uma definição recursiva da função (pré-definida) drop :: Int -> [a] -> [a] que
-- dado um inteiro n e uma lista l calcula a lista sem os (no máximo) n primeiros elementos de l.
-- Se a lista fornecida tiver n elementos ou menos, a lista resultante será vazia.
-- Por exemplo, drop 2 [10,20,30] corresponde a [30].

myDrop :: Int -> [a] -> [a]
myDrop _ [] = []
myDrop 0 l = l
myDrop n (h : t) = myDrop (n-1) t
