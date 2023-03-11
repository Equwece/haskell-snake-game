module Game.Canvas.Core where

import Control.Lens
import Control.Monad (forM_)
import Data.Default
import Data.Typeable (cast)
import Defaults (cellLength, startBoard, startGoal, startSnake)
import Game.Canvas.ChangeDirection (changeDirection)
import Game.Canvas.MoveSnake (createMoveSnakeNewState, getResetedGameState)
import Game.GameTypes
import Game.Lenses
import Monomer.Graphics.ColorTable
import qualified Monomer.Lens as L
import Monomer.Widgets.Single

canvas :: WidgetNode s AppEvent
canvas = canvas_ def

canvas_ :: [CanvasCfg] -> WidgetNode s AppEvent
canvas_ configs = defaultWidgetNode "canvas" newWidget
  where
    config = mconcat configs
    state = GameState {_snake = startSnake, _goal = startGoal, _board = startBoard}
    newWidget = makeCanvas config state

makeCanvas :: CanvasCfg -> GameState -> Widget s AppEvent
makeCanvas cfg state = widget
  where
    widget =
      createSingle
        state
        def
          { singleMerge = merge,
            singleHandleEvent = handleEvent,
            singleHandleMessage = handleMessage,
            singleGetSizeReq = getSizeReq,
            singleRender = render,
            singleResize = resize
          }

    resize wenv node (Rect rx ry _ _) = result
      where
        newNode = node & L.info . L.viewport .~ Rect rx ry 400 400
        result = resultNode newNode

    merge wenv node oldNode oldState = result
      where
        newNode =
          node
            & L.widget .~ makeCanvas cfg oldState
        result = resultNode newNode

    handleEvent wenv node target evt = case evt of
      _anyOther -> Nothing

    handleMessage wenv node target msg = case cast msg of
      Just ResetGame -> Just result
        where
          newState = getResetedGameState state
          newNode =
            node
              & L.widget .~ makeCanvas cfg newState
          result = resultNode newNode
      Just (MoveSnake newGen) -> Just result
        where
          vp = node ^. L.info . L.viewport
          vpHeight = vp ^. rH
          vpWidth = vp ^. rW
          newState = createMoveSnakeNewState state newGen cellLength vpWidth vpHeight
          newNode =
            node
              & L.widget .~ makeCanvas cfg newState
          result
            | (newState ^. snake . score) == (state ^. snake . score) = resultReqs newNode [RenderOnce]
            | otherwise = resultReqsEvts newNode [RenderOnce] [AppUpdateScore (newState ^. snake . score)]
      Just (ChangeDirection dir) -> Just result
        where
          newState = changeDirection state dir cellLength
          newNode =
            node
              & L.widget .~ makeCanvas cfg newState
          result = resultNode newNode
      _anyOther -> Nothing

    getSizeReq wenv node = (reqW, reqH)
      where
        reqW = minWidth 100
        reqH = minHeight 100

    render :: WidgetEnv s e -> WidgetNode s e -> Renderer -> IO ()
    render wenv node renderer = do
      drawInTranslation renderer origin $ do
        forM_
          (state ^. snake . elems)
          renderSegment

        renderGoal

        forM_
          rowBorderLines
          renderGridDimension

        forM_
          columnBorderLines
          renderGridDimension
      where
        vp = node ^. L.info . L.viewport
        rw = state ^. snake . chunkWidth
        rh = state ^. snake . chunkHeight
        snakeGoal = Rect (view (goal . positionX) state) (view (goal . positionY) state) rw rh

        origin = Point (vp ^. L.x) (vp ^. L.y)

        rowBorderLines = [((state ^. board . leftBorder, state ^. board . upBorder), (state ^. board . rightBorder, state ^. board . upBorder)), ((state ^. board . leftBorder, state ^. board . bottomBorder), (state ^. board . rightBorder, state ^. board . bottomBorder))]

        columnBorderLines = [((state ^. board . leftBorder, state ^. board . upBorder), (state ^. board . leftBorder, state ^. board . bottomBorder)), ((state ^. board . rightBorder, state ^. board . upBorder), (state ^. board . rightBorder, state ^. board . bottomBorder))]

        renderSegment (Segment (Point startX startY) (Point endX endY) d) = do
          setFillColor renderer black
          beginPath renderer
          renderRect renderer newSegmentRect
          fill renderer
          where
            diffCoordX = abs (startX - endX)
            diffCoordY = abs (startY - endY)
            newSegmentRect
              | d == 0 = Rect startX startY rw diffCoordY
              | d == 2 = Rect endX endY rw diffCoordY
              | d == 1 = Rect endX endY diffCoordX rh
              | otherwise = Rect startX startY diffCoordX rh

        renderGridDimension line = do
          setStrokeColor renderer orangeRed
          setStrokeWidth renderer 1
          beginPath renderer
          renderLine renderer (Point ((fst . fst) line) ((snd . fst) line)) (Point ((fst . snd) line) ((snd . snd) line))
          stroke renderer
          return ()

        renderGoal = do
          beginPath renderer
          setFillColor renderer red
          renderEllipse renderer snakeGoal
          fill renderer
