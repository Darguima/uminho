-- 12. Apresente uma definição recursiva da função (pré-definida) concat :: [[a]] -> [a] que
-- concatena as listas de uma lista.
-- Por exemplo, concat [[1],[2,2],[3],[4,4,4],[5],[4]] corresponde a [1,2,2,3,4,4,4,5,4].

import Ex03 ( (+++) )

concatenate :: [[a]] -> [a]
concatenate [] = []
concatenate [[]] = []
concatenate (h : t) = h +++ concatenate t
