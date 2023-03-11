{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Game.Canvas.MoveSnake (createMoveSnakeNewState, getResetedGameState) where

import Control.Lens
import Control.Monad (foldM)
import Data.Fixed (div', mod')
import Defaults (cellLength, startGoal, startSnake)
import Game.Canvas.ChangeDirection (getCellXCoord, getCellYCoord, getSnakeWithNewSegments)
import Game.GameTypes
import Game.Lenses
import Monomer.Common.BasicTypes (Point (Point), Rect (Rect))
import System.Random (Random (random, randoms), StdGen)

getRandomPoints :: StdGen -> Double -> Double -> Double -> Int -> [(Double, Double)]
getRandomPoints gen boardWidth boardHeight cellLength count = createPoints randCoords
  where
    randNumbs = take (count * 2) $ randoms gen :: [Int]
    xCoords = map (fromIntegral . (* round cellLength) . (`div` round cellLength) . (`mod` round boardWidth)) (take count randNumbs) :: [Double]
    yCoords = map (fromIntegral . (* round cellLength) . (`div` round cellLength) . (`mod` round boardHeight)) (drop count randNumbs) :: [Double]
    constructXYList (fx : xs) (fy : ys) = fx : fy : constructXYList xs ys
    constructXYList [] [] = []
    constructXYList _ _ = []
    randCoords = constructXYList xCoords yCoords
    createPoints (cX : cY : xs) = (cX, cY) : createPoints xs
    createPoints [] = []
    createPoints _ = []

getNewGoalCoord :: StdGen -> Double -> Double -> Double -> Snake -> Int -> (Double, Double)
getNewGoalCoord gen boardWidth boardHeight cellLength currentSnake count = newGoalCoord
  where
    (coordX, coordY) = last $ getRandomPoints gen boardWidth boardHeight cellLength count
    newGoalCoord
      | not $ isPointEnterSnake (Point coordX coordY) (currentSnake ^. elems) (currentSnake ^. chunkWidth) (currentSnake ^. chunkHeight) = (coordX, coordY)
      | otherwise = getNewGoalCoord gen boardWidth boardHeight cellLength currentSnake (count + 1)
getNewGoalCoord _ _ _ _ _ 50 = (0, 0)

setCoords :: Int -> Double -> (Double, Double)
setCoords d snakeStep
  | d == 2 = (0, snakeStep)
  | d == 1 = (snakeStep, 0)
  | d == 0 = (0, -1 * snakeStep)
  | otherwise = (-1 * snakeStep, 0)

updateElems :: [Segment] -> Snake -> [Segment]
updateElems el s = newElems
  where
    originElemsLen = length el
    firstElem = head el
    newFirstElem = firstElem & start . pX +~ fst (setCoords (firstElem ^. segmentDir) (s ^. step)) & start . pY +~ snd (setCoords (firstElem ^. segmentDir) (s ^. step))

    elemsWithUpdatedFirstElem = el & ix 0 .~ newFirstElem

    lastElem = last elemsWithUpdatedFirstElem
    newLastElem = lastElem & end . pX +~ fst (setCoords (lastElem ^. segmentDir) (s ^. step)) & end . pY +~ snd (setCoords (lastElem ^. segmentDir) (s ^. step))

    newElems
      | not $ isAlmostSame (lastElem ^. start) (lastElem ^. end) = elemsWithUpdatedFirstElem & ix (originElemsLen - 1) .~ newLastElem
      | otherwise = take (originElemsLen - 1) elemsWithUpdatedFirstElem

isAlmostSame :: Point -> Point -> Bool
isAlmostSame p1 p2
  | round (p1 ^. pX) == round (p2 ^. pX) && round (p1 ^. pY) == round (p2 ^. pY) = True
  | otherwise = False

isSnakeGotGoal :: Segment -> Goal -> Bool
isSnakeGotGoal (Segment (Point startX startY) _ segDir) (Goal goalX goalY goalW goalH) = result
  where
    result
      | (segDir == 0 || segDir == 2) && isPointEnterRectangle (Point (startX + 1) startY) (Rect goalX goalY goalW goalH) = True
      | (segDir == 1 || segDir == 3) && isPointEnterRectangle (Point startX (startY + 1)) (Rect goalX goalY goalW goalH) = True
      | otherwise = False

isSnakeGotWall :: Segment -> Board -> Bool
isSnakeGotWall (Segment (Point startX startY) _ segDir) (Board borderUp borderDown borderRight borderLeft) = result
  where
    result
      | segDir == 0 && startY < (borderUp - 0.1) = True
      | segDir == 2 && startY > (borderDown + 0.1) = True
      | segDir == 1 && startX > (borderRight + 0.1) = True
      | segDir == 3 && startX < (borderLeft - 0.1) = True
      | otherwise = False

isSnakeGotItself :: Snake -> Bool
isSnakeGotItself currentSnake = result $ currentSnake ^. elems
  where
    result ((Segment (Point fStartX fStartY) _ segDir) : _ : reminderSegments) = finalResult
      where
        rw = currentSnake ^. chunkWidth
        rh = currentSnake ^. chunkHeight
        (coordX, coordY)
          | segDir == 0 || segDir == 2 = (fStartX + 1, fStartY)
          | otherwise = (fStartX, fStartY + 1)
        finalResult = isPointEnterSnake (Point coordX coordY) reminderSegments rw rh
    result (_ : _) = False
    result [] = False

isPointEnterSnake :: Point -> [Segment] -> Double -> Double -> Bool
isPointEnterSnake (Point pointX pointY) segments rw rh = result
  where
    checkPointGotOneSeg (Segment (Point startX startY) (Point endX endY) segDir) = checkResult
      where
        diffCoordX = abs (startX - endX)
        diffCoordY = abs (startY - endY)
        segRect
          | segDir == 0 = Rect startX startY rw diffCoordY
          | segDir == 2 = Rect endX endY rw diffCoordY
          | segDir == 1 = Rect endX endY diffCoordX rh
          | otherwise = Rect startX startY diffCoordX rh
        checkResult = isPointEnterRectangle (Point pointX pointY) segRect
    middleList = map checkPointGotOneSeg segments
    result
      | or middleList = True
      | otherwise = False

isPointEnterRectangle :: Point -> Rect -> Bool
isPointEnterRectangle (Point pointX pointY) (Rect rectX rectY rectWidth rectHeight) = result
  where
    result
      | pointX >= rectX && pointX <= (rectX + rectWidth) && pointY >= rectY && pointY <= (rectY + rectHeight) = True
      | otherwise = False

getResetedGameState :: GameState -> GameState
getResetedGameState state = state & goal .~ startGoal & snake .~ startSnake

increaseSnakeLength :: Snake -> Double -> Snake
increaseSnakeLength currentSnake cellLength = newSnake
  where
    snakeElems = currentSnake ^. elems
    lastElem = last snakeElems
    elemDir = lastElem ^. segmentDir
    elemsLength = length snakeElems
    newLastElem
      | elemDir == 0 = lastElem & end . pY +~ cellLength
      | elemDir == 2 = lastElem & end . pY -~ cellLength
      | elemDir == 1 = lastElem & end . pX -~ cellLength
      | otherwise = lastElem & end . pX +~ cellLength
    newSnakeElems = snakeElems & ix (elemsLength - 1) .~ newLastElem
    newSnake = currentSnake & elems .~ newSnakeElems & score +~ 1

createMoveSnakeNewState :: GameState -> StdGen -> Double -> Double -> Double -> GameState
createMoveSnakeNewState state randomGen cellLength vpWidth vpHeight = newState
  where
    currentSnake = state ^. snake
    currentElems = currentSnake ^. elems
    firstElem = head currentElems
    currentSnakeX = firstElem ^. start . pX
    currentSnakeY = firstElem ^. start . pY
    currentSnakeDir = firstElem ^. segmentDir
    snakeWidth = currentSnake ^. chunkWidth
    snakeHeight = currentSnake ^. chunkHeight

    (scaledBoardWidth, scaledBoardHeight) = (getCellXCoord vpWidth cellLength * cellLength, getCellYCoord vpHeight cellLength * cellLength)

    currentBoard = state ^. board
    scaledBoard = currentBoard & bottomBorder .~ scaledBoardHeight & rightBorder .~ scaledBoardWidth

    currentGoal = state ^. goal

    boardWidth = abs $ currentBoard ^. leftBorder - (state ^. board . rightBorder)
    boardHeight = abs $ currentBoard ^. upBorder - (state ^. board . bottomBorder)
    (newGoalX, newGoalY) = getNewGoalCoord randomGen boardWidth boardHeight cellLength currentSnake 1

    turnSnake (Nothing, _) = currentSnake & elems .~ updateElems currentElems currentSnake
    turnSnake (Just turnPoint, newDir)
      | isAlmostSame turnPoint headCenter = getSnakeWithNewSegments state newDir turnPoint
      | otherwise = currentSnake & elems .~ updateElems currentElems currentSnake
      where
        headCenter
          | currentSnakeDir == 1 = Point (currentSnakeX - (snakeWidth * 0.5)) (currentSnakeY + (snakeHeight * 0.5))
          | currentSnakeDir == 0 = Point (currentSnakeX + (snakeWidth * 0.5)) (currentSnakeY + (snakeHeight * 0.5))
          | currentSnakeDir == 3 = Point (currentSnakeX + (snakeWidth * 0.5)) (currentSnakeY + (snakeHeight * 0.5))
          | otherwise = Point (currentSnakeX + (snakeWidth * 0.5)) (currentSnakeY - (snakeWidth * 0.5))
    turnedSnake = turnSnake (currentSnake ^. nextTurnPoint)
    (newGoal, checkedGoalSnake)
      | isSnakeGotGoal firstElem currentGoal = (currentGoal & positionX .~ newGoalX & positionY .~ newGoalY, increaseSnakeLength turnedSnake cellLength)
      | otherwise = (currentGoal, turnedSnake)
    newState
      | isSnakeGotWall firstElem currentBoard || isSnakeGotItself checkedGoalSnake = getResetedGameState state
      | otherwise = state & snake .~ checkedGoalSnake & goal .~ newGoal & board .~ scaledBoard
