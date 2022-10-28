-- 30. Apresente uma definição recursiva da função (pré-definida) intersect :: Eq a => [a] -> [a] -> [a]
-- que retorna a lista resultante de remover da primeira lista os elementos que não
-- pertencem à segunda. Por exemplo, intersect [1,1,2,3,4] [1,3,5] corresponde a [1,1,3].

intersect :: Eq a => [a] -> [a] -> [a]
intersect _ [] = []
intersect [] _ = []
intersect (h : t) occurrenceList
  | h `elem` occurrenceList = h : intersect t occurrenceList
  | otherwise = intersect t occurrenceList
