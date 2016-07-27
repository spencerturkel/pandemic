{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
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

runNotifications :: (Monad m, Interpreter m) => WriterT [Notification] m a -> m a
runNotifications (WriterT mx) = do
  (x, ns) <- mx
  notify ns
  return x

instance (MonadTrans t, Monad m, Interpreter m) => Interpreter (t m) where
  showTarget = lift . showTarget
  getAction = lift . getAction
  getCard = lift . getCard
  notify = lift . notify
