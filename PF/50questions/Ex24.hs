-- 24. Apresente uma definição recursiva da função (pré-definida) isSubsequenceOf :: Eq a => [a] -> [a] -> Bool 
-- que testa se os elementos de uma lista ocorrem noutra pela mesma
-- ordem relativa.
-- Por exemplo, isSubsequenceOf [20,40] [10,20,30,40] corresponde a True enquanto que
-- isSubsequenceOf [40,20] [10,20,30,40] corresponde a False.

myIsSubsequenceOf :: Eq a => [a] -> [a] -> Bool 
myIsSubsequenceOf [] _ = True
myIsSubsequenceOf _ [] = False
myIsSubsequenceOf (subSequenceHead : subSequenceTail) (h : t)
  | subSequenceHead == h = myIsSubsequenceOf subSequenceTail t
  | otherwise = myIsSubsequenceOf (subSequenceHead : subSequenceTail) t
