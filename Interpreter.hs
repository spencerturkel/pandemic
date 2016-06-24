module Interpreter where

import Control.Monad.State

import Action
import PlayerCard
import Target

--data Interpreter m = Interpreter {
class Monad m => Interpreter m where
    showTarget :: Target -> m ()
    getAction :: Target -> m Action
    getCard :: m PlayerCard
--    }
