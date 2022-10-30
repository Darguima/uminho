-- 36. Defina a função preCrescente :: Ord a => [a] -> [a] calcula o maior prefixo crescente
-- de uma lista.
-- Por exemplo, preCrescente [3,7,9,6,10,22] corresponde a [3,7,9].

preCrescente :: Ord a => [a] -> [a]
preCrescente [] = []
preCrescente [e] = [e]
preCrescente (a : b : t)
  | a <= b = a :  preCrescente (b : t)
  | otherwise = [a]
