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
    getCard :: Target -> m PlayerCard

instance (MonadTrans t, Monad (t m), Interpreter m) => Interpreter (t m) where
  showTarget = lift . showTarget
  getAction = lift . getAction
  getCard = lift . getCard
