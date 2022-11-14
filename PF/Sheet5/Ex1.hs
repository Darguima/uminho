-- takeWhile :: (a->Bool) -> [a] -> [a] que determina os primeiros elementos
-- da lista que satisfazem um dado predicado; por exemplo:
-- takeWhile odd [1,3,4,5,6,6] == [1,3].

takeWhile_ :: (a->Bool) -> [a] -> [a]
takeWhile_  _ [] = []
takeWhile_  function (h : t)
  | function h = h : takeWhile_ function t
  | otherwise = []

--  dropWhile :: (a->Bool) -> [a] -> [a] que elimina os primeiros elementos da
-- lista que satisfazem um dado predicado; por exemplo:
-- dropWhile odd [1,3,4,5,6,6] == [4,5,6,6]

dropWhile_ :: (a->Bool) -> [a] -> [a]
dropWhile_  _ [] = []
dropWhile_  function (h : t)
  | function h = dropWhile_ function t
  | otherwise = h : t


-- span :: (a-> Bool) -> [a] -> ([a],[a]), que calcula simultaneamente os dois
-- resultados anteriores. Note que apesar de poder ser definida à custa das outras
-- duas, usando a definição
-- span p l = (takeWhile p l, dropWhile p l)
-- nessa definição há trabalho redundante que pode ser evitado. Apresente uma
-- definição alternativa onde não haja duplicação de trabalho.

-- span_ :: (a-> Bool) -> [a] -> ([a],[a])
-- span_ function [] = ([], [])
-- span_ function (h : t)
--   | function h = (h : takes, drops)
--   | otherwise = (takes, h : drops)

--   where (takes, drops) = span_ function t
