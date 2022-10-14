-- 21. Apresente uma definição recursiva da função, isPrime :: Int -> Bool
-- que dado um número inteiro maior ou igual a 2 determina se esse número é primo. Para
-- determinar se um número n é primo, descubra se existe algum número inteiro m tal que
-- 2 ≤ m ≤ sqrt(n) e mod n m = 0. Se um tal número não existir então n é primo, e se existir então n
-- não é primo.

isPrime :: Int -> Bool
isPrime n
  | n < 2 = False
  | otherwise = verifyIfIsPrime n n

verifyIfIsPrime :: Int -> Int -> Bool

{-

n = 25

2 <= m <= 5

m:
2
3
4
5

mod 25 m = 0


-}