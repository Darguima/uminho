-- 5. Apresente uma definição recursiva da função (pré-definida) reverse :: [a] -> [a] que
-- dada uma lista calcula uma lista com os elementos dessa lista pela ordem inversa.
-- Por exemplo, reverse [10,20,30] corresponde a [30,20,10].

-- import ex3.(+++)
import Ex3

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (h : t) = myReverse t Ex3.+++ [h]
