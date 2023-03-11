{-# LANGUAGE TemplateHaskell #-}

module Game.Lenses where

import Control.Lens
import Game.GameTypes
import Monomer.Common.BasicTypes (Point (Point), Rect(..))

makeLenses 'GameState
makeLenses 'Goal
makeLenses 'Snake
makeLenses 'Board
makeLenses 'Turn
makeLenses 'Segment
makeLenses 'Point
makeLenses 'Rect
makeLenses 'AppModel
