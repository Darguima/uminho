-- 35. Apresente uma definição recursiva da função (pré-definida) lookup :: Eq a => a -> [(a,b)] -> Maybe b
-- que retorna uma lista construı́da a partir de elementos de uma lista (o segundo
-- argumento) atendendo a uma condição dada pelo primeiro argumento.
-- Por exemplo, lookup 'a' [('a',1),('b',4),('c',5)] corresponde à lista Just 1.

-- data Maybe a = Nothing | Just a
myLookup :: Eq a => a -> [(a,b)] -> Maybe b
myLookup _ [] = Nothing
myLookup element ((a, b) : t)
  | element == a = Just b
  | otherwise = myLookup element t
