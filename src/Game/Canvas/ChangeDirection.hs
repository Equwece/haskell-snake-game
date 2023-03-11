{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Game.Canvas.ChangeDirection (changeDirection, getSnakeWithNewSegments, getCellXCoord, getCellYCoord) where

import Control.Lens
import Data.Fixed (div')
import Game.GameTypes
import Game.Lenses
import Monomer.Common.BasicTypes (Point (Point))

getCellXCoord :: Double -> Double -> Double
getCellXCoord currentSnakeX cellLength = fromIntegral $ currentSnakeX `div'` cellLength

getCellYCoord :: Double -> Double -> Double
getCellYCoord currentSnakeY cellLength = fromIntegral $ currentSnakeY `div'` cellLength

changeDirection :: GameState -> Int -> Double -> GameState
changeDirection state newDir cellLength = newState
  where
    currentSnake = state ^. snake
    currentElems = currentSnake ^. elems
    firstElem = head currentElems
    currentSnakeX = firstElem ^. start . pX
    currentSnakeY = firstElem ^. start . pY
    currentSnakeDir = firstElem ^. segmentDir
    rx = (getCellXCoord currentSnakeX cellLength * cellLength) + (currentSnake ^. chunkWidth * 0.5)
    ry = (getCellYCoord currentSnakeY cellLength * cellLength) + (currentSnake ^. chunkHeight * 0.5)
    newTurn = (Just (Point rx ry), newDir)
    newSnake
      | currentSnakeDir /= newDir && abs (currentSnakeDir - newDir) /= 2 = currentSnake & nextTurnPoint .~ newTurn
      | otherwise = currentSnake
    newState = state & snake .~ newSnake


getSnakeWithNewSegments :: GameState -> Int -> Point -> Snake
getSnakeWithNewSegments state newDir (Point turnPointX turnPointY) = newSnake
  where
    currentSnake = state ^. snake
    currentElems = currentSnake ^. elems
    firstElem = head currentElems
    snakeWidth = currentSnake ^. chunkWidth
    snakeHeight = currentSnake ^. chunkHeight
    currentSnakeDir = firstElem ^. segmentDir
    (currentSnakeX, currentSnakeY)
      | currentSnakeDir == 1 = (turnPointX + (snakeWidth * 0.5), turnPointY - (snakeHeight * 0.5))
      | currentSnakeDir == 0 = (turnPointX - (snakeWidth * 0.5), turnPointY - (snakeHeight * 0.5))
      | currentSnakeDir == 3 = (turnPointX - (snakeWidth * 0.5), turnPointY - (snakeHeight * 0.5))
      | otherwise = (turnPointX - (snakeWidth * 0.5), turnPointY + (snakeWidth * 0.5))
    currentPoint = Point currentSnakeX currentSnakeY
    defaultSegment = Segment {_start = currentPoint, _end = currentPoint, _segmentDir = newDir}

    (newSegment, newCurrentSegment)
      | newDir == 0 = (upResolver, upPreviousResolver)
      | newDir == 1 = (rightResolver, rightPreviousResolver)
      | newDir == 2 = (downResolver, downPreviousResolver)
      | otherwise = (leftResolver, leftPreviousResolver)
      where
        (upResolver, upPreviousResolver)
          | currentSnakeDir == 3 = (defaultSegment & end . pY +~ snakeHeight, firstElem & start . pX +~ snakeWidth)
          | otherwise = (defaultSegment & end . pY +~ snakeHeight & start . pX -~ snakeWidth & end . pX -~ snakeWidth, firstElem & start . pX -~ snakeWidth)
        (downResolver, downPreviousResolver)
          | currentSnakeDir == 3 = (defaultSegment & start . pY +~ snakeHeight, firstElem & start . pX +~ snakeWidth)
          | otherwise = (defaultSegment & start . pX -~ snakeWidth & end . pX -~ snakeWidth & start . pY +~ snakeHeight, firstElem & start . pX -~ snakeWidth)
        (rightResolver, rightPreviousResolver)
          | currentSnakeDir == 0 = (defaultSegment & start . pX +~ snakeWidth, firstElem & start . pY +~ snakeHeight)
          | otherwise = (defaultSegment & start . pX +~ snakeWidth & start . pY -~ snakeHeight & end . pY -~ snakeHeight, firstElem & start . pY -~ snakeHeight)
        (leftResolver, leftPreviousResolver)
          | currentSnakeDir == 0 = (defaultSegment & end . pX +~ snakeWidth, firstElem & start . pY +~ snakeHeight)
          | otherwise = (defaultSegment & end . pX +~ snakeWidth & start . pY -~ snakeHeight & end . pY -~ snakeHeight, firstElem & start . pY -~ snakeHeight)
    newSnake
      | newDir /= currentSnakeDir && abs (currentSnakeDir - newDir) /= 2 = currentSnake & elems .~ newSegment : (currentElems & ix 0 .~ newCurrentSegment) & nextTurnPoint .~ (Nothing, 0)
      | otherwise = currentSnake
