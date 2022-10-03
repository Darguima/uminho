-- Defina uma função recursiva que recebe uma lista de inteiros e adiciona um valor dado a cada elemento da lista.

sumList :: Num a => [a] -> a -> [a]
sumList [] _ = []
sumList (head: tail) c = (head + c) : sumList tail c
