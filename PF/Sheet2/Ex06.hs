-- threeLast :: [a] -> [a] devolve os últimos três elementos de uma lista. Se a
-- lista de entrada tiver menos de três elementos, devolve a própria lista.

threeLast :: [a] -> [a]
threeLast [] = []
threeLast [a, b, c] = [a, b, c]
threeLast (a : b :c : t) = threeLast(b : c : t)
threeLast l = l
