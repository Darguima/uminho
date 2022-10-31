-- 50. Considere o seguinte tipo para representar o estado de um equipamento.
-- data Equipment = Bom | Razoavel | Avariado
-- deriving Show
-- Defina a função notRepair :: [Equipment] -> Int que determina a quantidade de
-- Equipments que não estão avariados.

data Equipment = Bom | Razoavel | Avariado
  deriving Show

notRepair :: [Equipment] -> Int
notRepair [] = 0
notRepair (Avariado : t) = notRepair t 
notRepair (_ : t) = 1 + notRepair t 
