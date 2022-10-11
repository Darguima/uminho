-- 9. Apresente uma definição recursiva da função (pré-definida) replicate :: Int -> a -> [a] 
-- que dado um inteiro n e um elemento x constói uma lista com n elementos, todos iguais
-- a x.
-- Por exemplo, replicate 3 10 corresponde a [10,10,10].

myReplicate :: Int -> a -> [a]
myReplicate 0 _ = []
myReplicate n element = element : myReplicate (n - 1) element
