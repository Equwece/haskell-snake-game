{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Core
  ( AppModel (..),
    AppEvent (..),
    buildUI,
    handleEvent,
  )
where

import Control.Concurrent (threadDelay)
import Control.Lens
import Data.Fixed (mod')
import Game.Canvas.Core
import Game.GameTypes
import Game.Lenses (snakeScore)
import Monomer (AppEventResponse, EventResponse (Message, Model, Producer), button, filler, gray, hstack, keystroke, label, spacer, vstack)
import Monomer.Widgets.Single
import System.Random
import TextShow (TextShow (showt))

buildUI ::
  WidgetEnv AppModel AppEvent ->
  AppModel ->
  WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree
  where
    withKeys =
      keystroke
        [ ("Up", AppKeyUp),
          ("Down", AppKeyDown),
          ("Right", AppKeyRight),
          ("Left", AppKeyLeft)
        ]
    widgetTree =
      withKeys $
        vstack
          [ hstack [button "Restart" AppResetGame, filler, label $ "Score: " <> showt (model ^. snakeScore)],
            spacer,
            canvas `nodeKey` "mainCanvas"
          ]
          `styleBasic` [padding 10]

handleEvent ::
  WidgetEnv AppModel AppEvent ->
  WidgetNode AppModel AppEvent ->
  AppModel ->
  AppEvent ->
  [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  AppResetGame -> [Message "mainCanvas" ResetGame]
  AppInit -> [Producer gameLoopProducer]
  AppMoveSnake gen -> [Message "mainCanvas" (MoveSnake gen)]
  AppKeyUp -> [Message "mainCanvas" (ChangeDirection 0)]
  AppKeyRight -> [Message "mainCanvas" (ChangeDirection 1)]
  AppKeyDown -> [Message "mainCanvas" (ChangeDirection 2)]
  AppKeyLeft -> [Message "mainCanvas" (ChangeDirection 3)]
  AppUpdateScore newScore -> [Model (model & snakeScore .~ newScore)]
  where
    gameLoopProducer sendMsg = do
      newGen <- newStdGen
      sendMsg (AppMoveSnake newGen)
      -- let r = head $ randoms newGen :: Int
      -- print $ (fromIntegral . (* 20) . (`div` 20) . (`mod` 400)) r
      threadDelay $ 1000 * 10
      gameLoopProducer sendMsg
