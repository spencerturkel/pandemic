{-# LANGUAGE FlexibleContexts #-}

module GameLoop where

import Control.Lens
import Control.Monad
import Control.Monad.Except
import Control.Monad.State

import DrawStage
import Exception
import Globals
import Interpreter
import Player
import RunAction
import Target

playerCycle :: Globals -> [Int]
playerCycle globals = cycle $ globals^..players.traversed.playerNumber

doValidAction :: (Interpreter m, MonadState Target m) => m ()
doValidAction = do
  target <- get
  action <- getAction target
  case runAction target action of
    Just newTarget -> do
      put newTarget
    Nothing ->
      doValidAction

showTargetAndDoNextAction :: (Interpreter m, MonadState Target m) => m ()
showTargetAndDoNextAction = do
  target <- get
  showTarget target
  doValidAction

run :: Interpreter m => Globals -> m (Target, Loseable)
run globalState = run' globalState (playerCycle globalState)
  where
    run' _ [] = undefined -- not possible
    run' globals (playerNum:tailPlayerNums) =
      let
        target :: Target
        target = (globals, playerNum)

        player :: Lens' Globals Player
        player = lens getter setter
          where
            getter g = g^.players.to (!! playerNum)
            setter g p = g & players.ix playerNum .~ p
      in do
      (g, _) <- runNotifications $ runActions target
      finalState <-
        runNotifications .
        runExceptT $
        execStateT (drawStage player *> doInfectionStep) g
      either
        (endGame (g, playerNum))
        (flip run' tailPlayerNums)
        finalState

runActions :: Interpreter m => Target -> m Target
runActions = execStateT $ replicateM 4 showTargetAndDoNextAction
