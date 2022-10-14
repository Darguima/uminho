-- 19. Defina a função filterAge :: Int -> Int -> [(String,Int)] -> [String] que recebe o ano,
-- a idade e uma lista de pares com o nome e o ano de nascimento de cada pessoa, e devolve a
-- listas de nomes das pessoas que nesse ano atingirão ou já ultrapassaram a idade indicada.
-- Por exemplo, idade 2021 26 [("rui",1995), ("maria",2009), ("ana",1947)] corresponde
-- a ["rui","ana"].

filterAge :: Int -> Int -> [(String,Int)] -> [String]
filterAge _ _ [] = []
filterAge year age ((name, birthdayYear) : t)
  | year - age >= birthdayYear = name : filterAge year age t
  | otherwise = filterAge year age t
