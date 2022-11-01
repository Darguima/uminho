-- 46. Considere o seguinte tipo para representar movimentos de um robot.
-- data Movimento = Norte | Sul | Este | Oeste
-- deriving Show
-- Defina a função directions :: (Int,Int) -> (Int,Int) -> [Direction] que, dadas as posições
-- inicial e final (coordenadas) do robot, produz uma lista de movimentos suficientes para que o
-- robot passe de uma posição para a outra.

import Prelude hiding (Right, Left)

type Position = (Int, Int)

data Direction = Up | Down | Left | Right
  deriving Show

directions :: Position -> (Int,Int) -> [Direction]
directions (x1, y1) (x2, y2)
  | x1 < x2 = Right : directions (x1 + 1, y1) (x2, y2)
  | x1 > x2 = Left : directions (x1 - 1, y1) (x2, y2)

  | y1 < y2 = Up : directions (x1, y1 + 1) (x2, y2)
  | y1 > y2 = Down : directions (x1, y1 - 1) (x2, y2)

  | x1 == x2 && y1 == y2 = []
