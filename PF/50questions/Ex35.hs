-- 35. Apresente uma definição recursiva da função (pré-definida) myLookup :: Eq a => a -> [(a,b)] -> Maybe b
-- que retorna uma lista construı́da a partir de elementos de uma lista (o segundo
-- argumento) atendendo a uma condição dada pelo primeiro argumento.
-- Por exemplo, myLookup ’a’ [(’a’,1),(’b’,4),(’c’,5)] corresponde à lista Just 1.

myLookup :: Eq a => a -> [(a,b)] -> Maybe b
myLookup _ [] = Nothing
myLookup searchElement ((a, b) : t)
  | a == searchElement = Just b
  | otherwise = myLookup searchElement t
