module Ex1 where

-- (a) any_ :: (a -> Bool) -> [a] -> Bool que teste se um predicado é verdade para algum elemento de uma lista;
-- por exemplo: any_ odd [1..10] == True

any_ :: (a -> Bool) -> [a] -> Bool
any_ _ [] = False
any_ function (h : t) = function h || any_ function t

-- (b) zipWith_ :: (a->b->c) -> [a] -> [b] -> [c] que combina os elementos de duas listas usando uma função específica;
-- por exemplo: zipWith_ (+) [1,2,3,4,5] [10,20,30,40] == [11,22,33,44].

zipWith_ :: (a->b->c) -> [a] -> [b] -> [c]
zipWith_ function (h1 : t1) (h2 : t2) = function h1 h2 : zipWith_ function t1 t2
zipWith_ _ _ _ = []

-- (c) takeWhile_ :: (a->Bool) -> [a] -> [a] que determina os primeiros elementos da lista que satisfazem um dado predicado;
-- por exemplo: takeWhile_ odd [1,3,4,5,6,6] == [1,3].

takeWhile_ :: (a->Bool) -> [a] -> [a]
takeWhile_ function (h : t)
  | function h = h : takeWhile_ function t
  | otherwise = []
takeWhile_ _ _ = []

-- (d) dropWhile_ :: (a->Bool) -> [a] -> [a] que elimina os primeiros elementos da lista que satisfazem um dado predicado;
-- por exemplo: dropWhile_ odd [1,3,4,5,6,6] == [4,5,6,6].

dropWhile_ :: (a->Bool) -> [a] -> [a]
dropWhile_ function (h : t)
  | function h = dropWhile_ function t
  | otherwise = h : t
dropWhile_ _ _ = []

-- (e) span_ :: (a-> Bool) -> [a] -> ([a],[a]), que calcula simultaneamente os dois resultados anteriores.
-- Note que apesar de poder ser definida à custa das outras duas, usando a definição span_ p l = (takeWhile_ p l, dropWhile_ p l)
-- nessa definição há trabalho redundante que pode ser evitado. Apresente uma definição alternativa onde não haja duplicação de trabalho.

span_ :: (a-> Bool) -> [a] -> ([a],[a])
span_ _ [] = ([], [])
span_ function (h : t)
  | function h = (h : takes, drops)
  | otherwise = ([], h : t)
  where (takes, drops) = span_ function t

-- (f) deleteBy_ :: (a -> a -> Bool) -> a -> [a] -> [a] que apaga o primeiro elemento de uma lista que é “igual” a um dado elemento
-- de acordo com a função de comparação que é passada como parâmetro. Por exemplo: deleteBy_ (\x y -> snd x == snd y) (1,2) [(3,3),(2,2),(4,2)]

deleteBy_ :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy_ _ _ [] = []
deleteBy_ function searchElement (h : t)
  | function searchElement h = t
  | otherwise = h : deleteBy_ function searchElement t

-- (g) sortOn_ :: Ord b => (a -> b) -> [a] -> [a] que ordena uma lista comparando os resultados de aplicar uma função
-- de extração de uma chave a cada elemento de uma lista. Por exemplo: sortOn_ fst [(3,1),(1,2),(2,5)] == [(1,2),(2,5),(3,1)].

sortOn_ :: Ord b => (a -> b) -> [a] -> [a]
sortOn_ _ [] = []
sortOn_ function (h : t) = sortOn_ function smallTail ++ [h] ++ sortOn_ function bigTail
  where (smallTail, bigTail) = smallAndBigger function (function h) t

        smallAndBigger :: Ord b => (a -> b) -> b -> [a] -> ([a], [a])
        smallAndBigger _ _ [] = ([], [])
        smallAndBigger function middleElement (h : t)
          | function h <= middleElement = (h : smalls, bigs)
          | otherwise = (smalls, h : bigs)
            where (smalls, bigs) = smallAndBigger function middleElement t
