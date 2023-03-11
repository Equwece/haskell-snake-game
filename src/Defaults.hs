module Defaults where

import Game.GameTypes
import Monomer.Common.BasicTypes (Point (Point))

cellLength :: Double
cellLength = 20

startElems :: [Segment]
startElems =
  [Segment {_start = Point 100 200, _end = Point 100 220, _segmentDir = 0}]

startSnake :: Snake
startSnake = Snake {_headPositionX = 20, _headPositionY = 20, _headDirection = 1, _step = 0.9, _chunkWidth = cellLength, _chunkHeight = cellLength, _score = 0, _chunks = [], _turnHistory = [], _elems = startElems, _nextTurnPoint = (Nothing, 0)}

startGoal :: Goal
startGoal = Goal {_positionX = 100, _positionY = 100, _goalWidth = cellLength, _goalHeight = cellLength}

startBoard :: Board
startBoard = Board {_upBorder = 0, _leftBorder = 0, _rightBorder = cellLength * 15, _bottomBorder = cellLength * 15}
