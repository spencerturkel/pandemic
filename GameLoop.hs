module GameLoop where

import Data.Foldable
import Control.Lens
import Control.Monad
import Control.Monad.Except

import Globals

loop g =
  let
    playerCycle = g^.players.to cycle
    f globals player = undefined
  in
    void . runExceptT $ foldlM f g playerCycle
