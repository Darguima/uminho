-- 33. Apresente uma definição recursiva da função (pré-definida) myUnlines :: [String] -> String que
-- junta todas as strings da lista numa só, separando-as pelo caracter ’\n’.
-- Por exemplo, unlines ["Prog", "Func"] corresponde a "Prog\nFunc\n".

myUnlines :: [String] -> String
myUnlines [] = ""
myUnlines (h : t) = h ++ "\n" ++ myUnlines t 

