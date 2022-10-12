-- 15. Defina a função heads :: [[a]] -> [a] que recebe uma lista de listas e produz a lista com
-- o primeiro elemento de cada lista.
-- Por exemplo, heads [[2,3,4],[1,7],[],[8,5,3]] corresponde a [2,1,8]

heads :: [[a]] -> [a]
heads [] = []
heads (currentList : otherLists)
  | null currentList = heads otherLists
  | otherwise = head currentList  : heads otherLists
