-- 34. Apresente uma definição recursiva da função pMaior :: Ord a => [a] -> Int que dada
-- uma lista não vazia, retorna a posição onde se encontra o maior elemento da lista. As posições
-- da lista começam em 0, i.e., a função deverá retornar 0 se o primeiro elemento da lista for o
-- maior.

pMaior :: Ord a => [a] -> Int
pMaior [] = error "empty list"
pMaior (h : t) = pMaiorAux 0 0 h (h : t)
  where pMaiorAux :: Ord a => Int -> Int -> a -> [a] -> Int
        pMaiorAux _ biggestIndex _ [] = biggestIndex
        pMaiorAux currentIndex biggestIndex biggestValue (h : t)
          | h <= biggestValue = pMaiorAux (currentIndex + 1) biggestIndex biggestValue t
          | otherwise = pMaiorAux (currentIndex + 1) currentIndex h t
