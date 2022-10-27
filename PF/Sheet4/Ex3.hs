-- 3. Defina a função divMod :: Integral a => a -> a -> (a, a) que calcula simultane-
-- amente a divisão e o resto da divisão inteira por subtracções sucessivas.

divMod :: Integral a => a -> a -> (a, a)
divMod dividend divider = divModAux dividend divider 0
  where divModAux :: Integral a => a -> a -> a -> (a, a)
        divModAux dividend divider result
          | dividend < divider = (result, dividend)
          | otherwise = divModAux (dividend - divider) divider (result + 1)