-- 14. Apresente uma definição recursiva da função (pré-definida) tails :: [a] -> [[a]] que
-- calcula a lista dos sufixos de uma lista.
-- Por exemplo, tails [1,2,3] corresponde a [[1,2,3],[2,3],[3],[]].

myTails :: [a] -> [[a]]
myTails [] = [[]]
myTails l = l : myTails (tail l) 
