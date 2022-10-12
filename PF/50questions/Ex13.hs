-- 13. Apresente uma definição recursiva da função (pré-definida) inits :: [a] -> [[a]] que
-- calcula a lista dos prefixos de uma lista.
-- Por exemplo, inits [11,21,13] corresponde a [[],[11],[11,21],[11,21,13]].

import Ex06 ( myTake )

myInits :: [a] -> [[a]] 
myInits [] = [[]]
myInits l = myInits (myTake (length l - 1) l) ++ [l]
