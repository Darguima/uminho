{-|
Module : Sheet5
Description : Módulo Haskell contendo exemplos de funções recursivas feitas no âmbito da disciplina de PF e usadas aqui para testar o Haddock e o HUnit
Copyright : Darguima <dsgdevbraga@gmail.com>;

Este módulo contem funções para:

1. Criar uma lista de __m__ expoentes de __n__.
2. Um filtro de idades
3. Um filtro de nomes
4. Uma função que junta os números iguais seguidos de um array
-}

module Sheet5 where

{- |A função powerEnumFrom gera um array com o número __n__ levantado de 0 até __m__.

== Exemplos de utilização:
>>> powerEnumFrom 2 4
[1,2,4,8,16]
-}

powerEnumFrom :: Int -> Int -> [Int]
powerEnumFrom _ 0 = [1]
powerEnumFrom n m = powerEnumFrom n (m - 1) ++ [n^m]

{- |A função filterAge filtra as pessoas de um lista de pares que tenham uma determinada idade num ano específico.
A função recebe um ano e a idade mínima que os selecionados precisam ter nesse ano, para além de uma lista de pares @(String, Int)@ com o nome e o ano de nascimento das pessoas.

== Exemplos de utilização:
>>> filterAge 2022 18 [("Ana", 2004), ("Rui", 2007), ("Amadeu", 2000)]
["Ana","Amadeu"]
-}

filterAge :: Int -> Int -> [(String,Int)] -> [String]
filterAge _ _ [] = []
filterAge year age ((name, birthdayYear) : t)
  | year - age >= birthdayYear = name : filterAge year age t
  | otherwise = filterAge year age t

{- |A função cola recebe uma lista de triplos com informação de pessoas e extrai o nome de todas elas.

== Exemplos de utilização:
>>> cola [("Ana", 3, 5), ("Rui", 7, 6), ("Amadeu", 5, 2)]
"AnaRuiAmadeu"
-}

cola :: [(String,b,c)] -> String
cola [] = ""
cola ((nome, _, _) : t) = nome ++ cola t

{- |A função myGroup recebe uma lista de números e agrupa os que são iguais.

== Exemplos de utilização:
>>> myGroup [1,2,2,3,4,4,4,5,4]
[[1],[2,2],[3],[4,4,4],[5],[4]]
-}

myGroup :: Eq a => [a] -> [[a]]
myGroup [] = []
myGroup (h : t) = joinSimilar h (myGroup t)
  where joinSimilar :: Eq a => a -> [[a]] -> [[a]]
        joinSimilar num [] = [[num]]
        joinSimilar num (h : t)
          | num == head h = (num : h) : t
          | otherwise = [num] : h : t
