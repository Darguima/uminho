-- 28. Apresente uma definição recursiva da função (pré-definida) (\\):: Eq a => [a] -> [a] -> [a]
-- que retorna a lista resultante de remover (as primeiras ocorrências) dos elementos da
-- segunda lista da primeira.
-- Por exemplo, (\\)[1,2,3,4,5,1] [1,5] corresponde a [2,3,4,1].

import Ex27 (delete)

(\\) :: Eq a => [a] -> [a] -> [a]
(\\) _ [] = []
(\\) [] l = l
(\\) occurrenceList (h : t)
  | h `elem` occurrenceList = delete h occurrenceList \\ t
  | otherwise = h : occurrenceList \\ t
