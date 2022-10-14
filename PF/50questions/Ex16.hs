-- 16. Defina a função total :: [[a]] -> Int que recebe uma lista de listas e conta o total de
-- elementos (de todas as listas)
-- Por exemplo, total [[2,3,4],[1,7],[],[8,5,3]] corresponde a 8.

total :: [[a]] -> Int
total [] = 0
total (h : t) = myLength h + total t
  where myLength :: [a] -> Int
        myLength [] = 0
        myLength (h : t) = 1 + myLength t
