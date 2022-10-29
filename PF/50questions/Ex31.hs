-- 31. Apresente uma definição recursiva da função (pré-definida) insert :: Ord a => a -> [a] -> [a]
-- que dado um elemento e uma lista ordenada retorna a lista resultante de inserir
-- ordenadamente esse elemento na lista.
-- Por exemplo, insert 25 [1,20,30,40] corresponde a [1,20,25,30,40].

insert :: Ord a => a -> [a] -> [a]
insert element [] = [element]
insert element (h : t)
  | h >= element = element : h : t
  | otherwise = h : insert element t
