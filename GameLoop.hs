{-# LANGUAGE FlexibleContexts #-}

module GameLoop where

import Control.Lens
import Control.Monad
import Control.Monad.Except
import Control.Monad.State

import DrawStage
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
    Just newTarget ->
      put newTarget
    Nothing ->
      doValidAction

showTargetAndDoNextAction :: (Interpreter m, MonadState Target m) => m ()
showTargetAndDoNextAction = do
  target <- get
  showTarget target
  doValidAction

run :: Interpreter m => Globals -> m Globals
run globalState = run' globalState (playerCycle globalState)
  where
    run' globals [] = return globals
    run' globals (playerNum:rest) =
      let
        target :: Target
        target = (globals, playerNum)

        player :: Lens' Globals Player
        player = lens getter setter
          where
            getter g = g^.players.to (!! playerNum)
            setter g p = g & players.ix playerNum .~ p
      in do
      g <- fst <$> runActions target
      finalState <- runExceptT $ execStateT (drawStage player *> doInfectionStep) g
      either
        (error . ("Got Loseable: " ++) . show)
        (`run'` rest)
        finalState

runActions :: Interpreter m => Target -> m Target
runActions = execStateT $ replicateM 4 showTargetAndDoNextAction
