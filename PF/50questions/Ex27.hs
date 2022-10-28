-- 27. Apresente uma definição recursiva da função (pré-definida) delete :: Eq a => a -> [a] -> [a] 
-- que retorna a lista resultante de remover (a primeira ocorrência de) um dado elemento
-- de uma lista.
-- Por exemplo, delete 2 [1,2,1,2,3,1,2] corresponde a [1,1,2,3,1,2]. Se não existir
-- nenhuma ocorrência a função deverá retornar a lista recebida.

module Ex27 where

delete :: Eq a => a -> [a] -> [a]
delete _ [] = []
delete occurrence (h : t)
  | occurrence == h = t
  | otherwise = h : delete occurrence t
