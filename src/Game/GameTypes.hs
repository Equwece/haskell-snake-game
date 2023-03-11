{-# LANGUAGE FlexibleContexts #-}

module Game.GameTypes
  ( Board (..),
    Snake (..),
    GameState (..),
    Goal (..),
    CanvasMessage (..),
    GameEvent (..),
    CanvasCfg (..),
    Turn (..),
    Segment (..),
    AppModel (..),
    AppEvent (..),
  )
where

import Data.Default
import Monomer.Common.BasicTypes (Point)
import System.Random (StdGen)

data CanvasCfg = CanvasCfg deriving (Eq, Show)

instance Default CanvasCfg where
  def = CanvasCfg

instance Semigroup CanvasCfg where
  (<>) _ _ = CanvasCfg

instance Monoid CanvasCfg where
  mempty = def

data CanvasMessage
  = ResetGame
  | MoveSnake {_randomGen :: !StdGen}
  | ChangeDirection !Int
  deriving (Eq, Show)

data GameEvent = GameInit | GameMoveSnake | GameUpdateExternalScore !Int deriving (Eq, Show)

data Goal = Goal
  { _positionX :: !Double,
    _positionY :: !Double,
    _goalWidth :: !Double,
    _goalHeight :: !Double
  }
  deriving (Eq, Show)

data GameState = GameState
  { _snake :: !Snake,
    _goal :: !Goal,
    _board :: !Board
  }
  deriving (Eq, Show)

data Snake = Snake
  { _headPositionX :: !Double,
    _headPositionY :: !Double,
    _chunkWidth :: !Double,
    _chunkHeight :: !Double,
    _headDirection :: !Int,
    _step :: !Double,
    _score :: !Int,
    _chunks :: ![(Point, Int, [Turn], Int)],
    _turnHistory :: ![(Point, Int)],
    _elems :: ![Segment],
    _nextTurnPoint :: !(Maybe Point, Int)
  }
  deriving (Eq, Show)

data Segment = Segment {_start :: Point, _end :: Point, _segmentDir :: Int} deriving (Eq, Show)

data Turn = Turn
  { _point :: !Point,
    _dir :: !Int
  }
  deriving (Eq, Show)

data Board = Board
  { _upBorder :: !Double,
    _bottomBorder :: !Double,
    _rightBorder :: !Double,
    _leftBorder :: !Double
  }
  deriving (Eq, Show)

data AppModel = AppModel
  { _snakeScore :: !Int
  }
  deriving (Eq, Show)

data AppEvent = AppResetGame | AppInit | AppMoveSnake {gen :: StdGen} | AppKeyUp | AppKeyDown | AppKeyLeft | AppKeyRight | AppUpdateScore !Int deriving (Eq, Show)
