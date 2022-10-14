-- 17. Defina a função fun :: [(a,b,c)] -> [(a,c)] que recebe uma lista de triplos e produz a
-- lista de pares com o primeiro e o terceiro elemento de cada triplo.
-- Por exemplo, fun [("rui",3,2), ("maria",5,2), ("ana",43,7)] corresponde a
-- [("rui",2), ("maria",2), ("ana",7)].

fun :: [(a,b,c)] -> [(a,c)]
fun [] = []
fun ((a, _, c) : t) = (a, c) : fun t
