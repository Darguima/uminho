-- 3. Apresente uma definição recursiva da função (pré-definida) (++) ::
--  [a] -> [a] -> [a]
-- que concatena duas listas.
-- Por exemplo, (++) [1,2,3] [10,20,30] corresponde à lista [1,2,3,10,20,30].

module Ex03 where

(+++) :: [a] -> [a] -> [a]
(+++) [] [] = []
(+++) l1 [] = l1
(+++) [] l2 = l2
(+++) (h : t) l2 = h : (t +++ l2 )
