-- Defina uma função que recebe uma lista de Strings e remove todas as Strings iniciadas por um dado caractere

filterList :: [String] -> Char -> [String]
filterList [] _ = []
filterList (a : b) c
  | a == "" = filterList b c
  | head a == c = filterList b c
  | otherwise = a : filterList b c
