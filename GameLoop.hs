module GameLoop where

import Data.Foldable
import Control.Lens
import Control.Monad
import Control.Monad.Except

import Globals
import Interpreter
import Player

interpret :: Interpreter m => Globals -> m Globals
interpret g = foldlM f g playerCycle
  where
    playerCycle = cycle $ g^..players.traversed.playerNumber

    f :: Interpreter m => Globals -> Int -> m Globals
    f globals playerNum = do
      let target = (globals, playerNum)
      showState target
      _
