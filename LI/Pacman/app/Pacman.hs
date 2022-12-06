module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

windowDimensions :: (Int, Int)
windowDimensions = (700, 700)

centerRadius :: (Int, Int)
centerRadius = (div x 2, div y 2)
  where (x, y) = windowDimensions

pacmanBmpSize :: Int
pacmanBmpSize = 256

pacmanSize :: Int
pacmanSize = 20

scalePacmanImage :: Float
scalePacmanImage = fromIntegral pacmanSize / fromIntegral  pacmanBmpSize

type Position = (Int, Int)
data CurrentDirection = DirStoped | DirRight | DirLeft | DirUp | DirDown
data WallsMode = WallStop | WallTeletransport

type FoodPos = [Position]

type State = (Position, CurrentDirection, WallsMode, FoodPos, [Picture])

initialState :: [Picture] -> State
initialState images = ((0, 0), DirStoped, WallStop, [(20, 20)], images)

drawNewState :: State -> IO Picture
drawNewState ((x, y), direction, _, foodsPos, pacmanSkin : _) = return $ Pictures (translate newX newY pacman : drawFoods foodsPos)
  where pacman :: Picture
        pacman = rotate (rotation direction) pacmanSkin

        newX = fromIntegral x
        newY = fromIntegral y

        drawFoods :: FoodPos -> [Picture]
        drawFoods = map (\(x, y) -> translate (fromIntegral x) (fromIntegral y) foodPosTemplate)
        foodPosTemplate = color red $ ThickCircle 0 5

        rotation :: CurrentDirection -> Float
        rotation DirRight = 0
        rotation DirDown = 90
        rotation DirLeft = 180
        rotation DirUp = 270
        rotation _ = 0

reactEvent :: Event -> State -> IO State
reactEvent (EventKey (SpecialKey KeyUp) Down _ _) (pos, _, wallsMode, foodsPos, i) = return (pos, DirUp, wallsMode, foodsPos, i)
reactEvent (EventKey (SpecialKey KeyDown) Down _ _) (pos, _, wallsMode, foodsPos, i) = return (pos, DirDown, wallsMode, foodsPos, i)
reactEvent (EventKey (SpecialKey KeyLeft) Down _ _) (pos, _, wallsMode, foodsPos, i) = return (pos, DirLeft, wallsMode, foodsPos, i)
reactEvent (EventKey (SpecialKey KeyRight) Down _ _) (pos, _, wallsMode, foodsPos, i) = return (pos, DirRight, wallsMode, foodsPos, i)

reactEvent (EventKey (SpecialKey KeySpace) Down _ _) (pos, cd, WallTeletransport, foodsPos, i) = return (pos, cd, WallStop, foodsPos, i)
reactEvent (EventKey (SpecialKey KeySpace) Down _ _) (pos, cd, WallStop, foodsPos, i) = return (pos, cd, WallTeletransport, foodsPos, i)

reactEvent _ s  = return s

movePacman :: State -> State
movePacman ((x, y), DirUp, wallsMode, foodsPos, i) = ((x, y + 5), DirUp, wallsMode, foodsPos, i)
movePacman ((x, y), DirDown, wallsMode, foodsPos, i) = ((x, y - 5), DirDown, wallsMode, foodsPos, i)
movePacman ((x, y), DirLeft, wallsMode, foodsPos, i) = ((x - 5, y), DirLeft, wallsMode, foodsPos, i)
movePacman ((x, y), DirRight, wallsMode, foodsPos, i) = ((x + 5, y ), DirRight, wallsMode, foodsPos, i)
movePacman currentState_ = currentState_

reactTime :: Float -> State -> IO State
reactTime _ currentState@(_, _, WallTeletransport, foodsPos, i) = return $ wallsTeletransport  (movePacman currentState)
  where wallsTeletransport :: State -> State
        wallsTeletransport currentState_@((x, y), cd, wallsMode, foodsPos, i)
          | -radiusX > x = ((radiusX, y), cd, wallsMode, foodsPos, i)
          | radiusX < x = ((-radiusX, y), cd, wallsMode, foodsPos, i)
          | -radiusY > y = ((x, radiusY), cd, wallsMode, foodsPos, i)
          | radiusY < y = ((x, -radiusY), cd, wallsMode, foodsPos, i)
          | otherwise = currentState_

          where (radiusX, radiusY) = centerRadius

reactTime _ currentState@(_, _, WallStop, _, _) = return $ windowWalls currentState (movePacman currentState)
  where windowWalls :: State -> State -> State
        windowWalls currentState_ newState@((newX, newY), _, _, _, _)
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
main = do pacmanSkin <- loadBMP "app/images/pacman.bmp"
          let images = [scale scalePacmanImage scalePacmanImage pacmanSkin]
          playIO dm
              black
              fr
              (initialState images)
              drawNewState
              reactEvent
              reactTime
