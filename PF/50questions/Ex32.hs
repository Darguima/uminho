-- 32. Apresente uma definição recursiva da função (pré-definida) myUnwords :: [String] -> String
-- que junta todas as strings da lista numa só, separando-as por um espaço.
-- Por exemplo, myUnwords ["Programacao", "Funcional"] corresponde a "Programacao Funcional".

myUnwords :: [String] -> String
myUnwords [] = ""
myUnwords [s] = s
myUnwords (h : t) = h ++ " " ++ myUnwords t 
