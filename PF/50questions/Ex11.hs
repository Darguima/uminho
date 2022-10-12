{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}

-- 11. Apresente uma definição recursiva da função (pré-definida) group :: Eq a => [a] -> [[a]] que
-- agrupa elementos iguais e consecutivos de uma lista.
-- Por exemplo, group [1,2,2,3,4,4,4,5,4] corresponde a [[1],[2,2],[3],[4,4,4],[5],[4]].

myGroup :: Eq a => [a] -> [[a]]
myGroup [] = []
myGroup (h : t) = joinSimilar h (myGroup t)

joinSimilar :: Eq a => a -> [[a]] -> [[a]]
joinSimilar num [] = [[num]]
joinSimilar num (h : t)
  | num == head h = (num : h) : t
  | otherwise = [num] : h : t
