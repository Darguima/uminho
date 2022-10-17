-- 22. Apresente uma definição recursiva da função (pré-definida) isPrefixOf :: Eq a => [a] -> [a] -> Bool que testa se uma lista é prefixo de outra.
-- Por exemplo, isPrefixOf [10,20] [10,20,30] corresponde a True enquanto que isPrefixOf
-- [10,30] [10,20,30] corresponde a False.

isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (prefixHead : prefixTail) (h : t)
  | prefixHead == h = isPrefixOf prefixTail t
  | otherwise = False
