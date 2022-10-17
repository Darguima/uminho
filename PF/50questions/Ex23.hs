-- 23. Apresente uma definição recursiva da função (pré-definida) isSuffixOf :: Eq a => [a] -> [a] -> Bool que testa se uma lista é sufixo de outra.
-- Por exemplo, isSuffixOf [20,30] [10,20,30] corresponde a True enquanto que isSuffixOf
-- [10,30] [10,20,30] corresponde a False.

myIsSuffixOf :: Eq a => [a] -> [a] -> Bool
myIsSuffixOf [] _ = True
myIsSuffixOf _ [] = False
myIsSuffixOf (suffixHead : suffixTail) (h : t)
  | length suffixTail < length t = myIsSuffixOf (suffixHead : suffixTail) t
  | suffixHead == h = myIsSuffixOf suffixTail t
  | otherwise = False
