{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Interpreter where

import Control.Monad.Writer.Strict

import Action
import Notification
import PlayerCard
import Target

class Interpreter m where
    showTarget :: Target -> m ()
    getAction :: Target -> m Action
    getCard :: Target -> m PlayerCard
    notify :: [Notification] -> m ()

instance (MonadTrans t, Monad m, Interpreter m) => Interpreter (t m) where
  showTarget = lift . showTarget
  getAction = lift . getAction
  getCard = lift . getCard
  notify = lift . notify
