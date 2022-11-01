-- 47. Consider o seguinte tipo de dados,
-- data Movimento = Norte | Sul | Este | Oeste
-- deriving Show
-- Defina a função hasLoops :: (Int,Int) -> [Movimento] -> Bool que dada uma posição
-- inicial e uma lista de movimentos (correspondentes a um percurso) verifica se o robot alguma
-- vez volta a passar pela posição inicial ao longo do percurso correspondente. Pode usar a
-- função posição definida acima.

import Prelude hiding (Right, Left)

type Position = (Int, Int)

data Direction = Up | Down | Left | Right
  deriving Show

walk :: Position -> Direction -> Position
walk (x, y) Up = (x, y + 1)
walk (x, y) Right = (x + 1, y)
walk (x, y) Left = (x - 1, y)
walk (x, y) Down = (x, y - 1)

hasLoops :: Position -> [Direction] -> Bool
hasLoops initialPos [] = False
hasLoops initialPos (firstMove : othersMoves) = hasLoopsAux initialPos (walk initialPos firstMove) othersMoves

  where hasLoopsAux :: Position -> Position -> [Direction] -> Bool

        hasLoopsAux (initialX, initialY) (currentX, currentY) []
          | initialX == currentX && initialY == currentY = True
          | otherwise = False

        hasLoopsAux (initialX, initialY) (currentX, currentY) (currentMove : othersMoves)
          | initialX == currentX && initialY == currentY = True
          | otherwise = hasLoopsAux initialPos (walk (currentX, currentY) currentMove) othersMoves