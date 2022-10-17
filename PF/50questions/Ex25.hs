-- 25. Apresente uma definição recursiva da função (pré-definida) elemIndices :: Eq a => a -> [a] -> [Int]
-- que calcula a lista de posições em que um dado elemento ocorre numa lista.
-- Por exemplo, elemIndices 3 [1,2,3,4,3,2,3,4,5] corresponde a [2,4,6].

elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices _ [] = []
elemIndices n l = elemIndicesAux n l 0

  where elemIndicesAux :: Eq a => a -> [a] -> Int -> [Int]
        elemIndicesAux _ [] _ = []
        elemIndicesAux n (h : t) index
          | n == h = index : elemIndicesAux n t (index + 1)
          | otherwise = elemIndicesAux n t (index + 1)
