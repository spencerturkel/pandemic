{-# LANGUAGE FlexibleContexts #-}

module GameLoop where

import Data.Foldable
import Control.Lens
import Control.Monad
import Control.Monad.Except
import Control.Monad.State

import Action
import DrawStage
import Globals
import Interpreter
import Player
import RunAction
import Target

runGame :: Interpreter m => Globals -> m Globals
runGame globals = run globals (playerCycle globals)

playerCycle :: Globals -> [Int]
playerCycle globals = cycle $ globals^..players.traversed.playerNumber

doValidAction :: (Interpreter m) => StateT Target m ()
doValidAction = do
  target <- get
  action <- lift $ getAction target
  case runAction target action of
    Just newTarget ->
      put newTarget
    Nothing ->
      doValidAction

showTargetAndDoNextAction :: (Interpreter m) => StateT Target m ()
showTargetAndDoNextAction = do
  target <- get
  lift $ showTarget target
  doValidAction

run :: Interpreter m => Globals -> [Int] -> m Globals
run globals = run' globals (playerCycle globals)
  where
    run' globals [] = return globals
    run' globals (playerNum:rest) =
      let
        target :: Target
        target = (globals, playerNum)

        playerLens :: Lens' Globals Player
        playerLens = lens get setter
          where
            get g = g^.players.to (!! playerNum)
            setter g p = g & players.ix playerNum .~ p
      in do
      g <- fst <$> runActions target
      finalState <- runExceptT $ execStateT (drawStage playerLens) g
      either undefined (`run` rest) finalState

runActions :: Interpreter m => Target -> m Target
runActions = execStateT $ replicateM 4 showTargetAndDoNextAction
