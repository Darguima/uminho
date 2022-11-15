module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

windowDimensions :: (Int, Int)
windowDimensions = (700, 700)

centerRadius :: (Int, Int)
centerRadius = (div x 2, div y 2)
  where (x, y) = windowDimensions

pacmanSize :: Int
pacmanSize = 20

type Position = (Int, Int)
data CurrentDirection = Dir_Stoped | Dir_Right | Dir_Left | Dir_Up | Dir_Down
data WallsMode = Wall_Stop | Wall_Teletransport

type State = (Position, CurrentDirection, WallsMode)

initialState :: State
initialState = ((0, 0), Dir_Stoped, Wall_Stop)

drawNewState :: State -> Picture
drawNewState ((x, y), _, _) = translate newX newY pacman
  where pacman :: Picture
        pacman = color yellow $ circle 20
        newX = fromIntegral x
        newY = fromIntegral y

reactEvent :: Event -> State -> State
reactEvent (EventKey (SpecialKey KeyUp) Down _ _) (pos, _, wallsMode) = (pos, Dir_Up, wallsMode)
reactEvent (EventKey (SpecialKey KeyDown) Down _ _) (pos, _, wallsMode) = (pos, Dir_Down, wallsMode)
reactEvent (EventKey (SpecialKey KeyLeft) Down _ _) (pos, _, wallsMode) = (pos, Dir_Left, wallsMode)
reactEvent (EventKey (SpecialKey KeyRight) Down _ _) (pos, _, wallsMode) = (pos, Dir_Right, wallsMode)

reactEvent (EventKey (SpecialKey KeySpace) Down _ _) (pos, cd, Wall_Teletransport) = (pos, cd, Wall_Stop)
reactEvent (EventKey (SpecialKey KeySpace) Down _ _) (pos, cd, Wall_Stop) = (pos, cd, Wall_Teletransport)

reactEvent _ s  = s

movePacman :: State -> State
movePacman ((x, y), Dir_Up, wallsMode) = ((x, y + 5), Dir_Up, wallsMode)
movePacman ((x, y), Dir_Down, wallsMode) = ((x, y - 5), Dir_Down, wallsMode)
movePacman ((x, y), Dir_Left, wallsMode) = ((x - 5, y), Dir_Left, wallsMode)
movePacman ((x, y), Dir_Right, wallsMode) = ((x + 5, y ), Dir_Right, wallsMode)
movePacman currentState_ = currentState_

reactTime :: Float -> State -> State
reactTime _ currentState@(_, _, Wall_Teletransport) = wallsTeletransport  (movePacman currentState)
  where wallsTeletransport :: State -> State
        wallsTeletransport currentState_@((x, y), cd, wallsMode)
          | -radiusX > x = ((radiusX, y), cd, wallsMode)
          | radiusX < x = ((-radiusX, y), cd, wallsMode)
          | -radiusY > y = ((x, radiusY), cd, wallsMode)
          | radiusY < y = ((x, -radiusY), cd, wallsMode)
          | otherwise = currentState_

          where (radiusX, radiusY) = centerRadius

reactTime _ currentState@(_, _, Wall_Stop) = windowWalls currentState (movePacman currentState)
  where windowWalls :: State -> State -> State
        windowWalls currentState_ newState@((newX, newY), _, _)
          | -radiusX + pacmanSize < newX && newX < radiusX - pacmanSize && -radiusY + pacmanSize < newY && newY < radiusY - pacmanSize = newState
          | otherwise = currentState_
          where (radiusX, radiusY) = centerRadius
                  
fr :: Int
fr = 50

dm :: Display
dm = InWindow
        "Pacman" 
       windowDimensions
       (200,200) 

main :: IO ()
main = do play dm
              black
              fr
              initialState
              drawNewState
              reactEvent
              reactTime
