{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Interpreter where

import Control.Monad.State

import Action
import PlayerCard
import Target

class Monad m => Interpreter m where
    showTarget :: Target -> m ()
    getAction :: Target -> m Action
    getCard :: m PlayerCard

instance (MonadTrans t, Monad (t m), Interpreter m) => Interpreter (t m) where
  showTarget = lift . showTarget
  getAction = lift . getAction
  getCard = lift getCard

instance Interpreter IO where
  showTarget = error "instance Interpreter IO not implemented"
  getAction = error "instance Interpreter IO not implemented"
  getCard = error "instance Interpreter IO not implemented"
