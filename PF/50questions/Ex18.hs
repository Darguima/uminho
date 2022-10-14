-- 18. Defina a função cola :: [(String,b,c)] -> String que recebe uma lista de triplos e con-
-- catena as strings que estão na primeira componente dos triplos.
-- Por exemplo, cola [("rui",3,2), ("maria",5,2), ("ana",43,7)] corresponde a "ruimariaana".

cola :: [(String,b,c)] -> String
cola [] = ""
cola ((nome, _, _) : t) = nome ++ cola t
