-- 45. Apresente uma definição recursiva da função pré-definida catMaybes :: [Maybe a] -> [a]
-- que colecciona os elementos do tipo a de uma lista.

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing : t) = catMaybes t
catMaybes (Just a : t) = a : catMaybes t
