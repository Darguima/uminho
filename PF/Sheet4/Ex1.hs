-- 1. Defina a função digitAlpha :: String -> (String,String), que dada uma string,
-- devolve um par de strings: uma apenas com as letras presentes nessa string, e a outra
-- apenas com os números presentes na string. Implemente a função de modo a fazer uma
-- única travessia da string. Relembre que as funções isDigit,isAlpha :: Char -> Bool
-- estão já definidas no módulo Data.Char.

import Data.Char ( isAlpha, isDigit )

digitAlpha :: String -> (String,String)
digitAlpha "" = ([], [])
digitAlpha (h : t)
  | isAlpha h = (h : alphas, digits)
  | isDigit h = (alphas, h : digits)
  | otherwise = (alphas, digits)
  where (alphas, digits) = digitAlpha t

-- digitAlpha with accumulator
digitAlphaAc :: String -> (String,String)
digitAlphaAc = digitAlphaAcAux ([], [])
  where digitAlphaAcAux :: (String,String) -> String -> (String,String)
        digitAlphaAcAux ac "" = ac
        digitAlphaAcAux (alphas, digits) (h : t)
          | isAlpha h = digitAlphaAcAux (alphas ++ [h], digits) t
          | isDigit h = digitAlphaAcAux (alphas, digits ++ [h]) t
          | otherwise = digitAlphaAcAux (alphas, digits) t
