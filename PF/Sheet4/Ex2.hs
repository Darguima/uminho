-- 2. Defina a função nzp :: [Int] -> (Int,Int,Int) que, dada uma lista de inteiros,
-- conta o número de valores nagativos, o número de zeros e o número de valores positivos,
-- devolvendo um triplo com essa informação. Certifique-se que a função que definiu
-- percorre a lista apenas uma vez.

nzp :: [Int] -> (Int,Int,Int)
nzp [] = (0, 0, 0)
nzp (h : t)
  | h < 0 = (negatives + 1, zeros, positives)
  | h == 0 = (negatives, zeros + 1, positives)
  | h > 0 = (negatives, zeros, positives + 1)
  where (negatives, zeros, positives) = nzp t
