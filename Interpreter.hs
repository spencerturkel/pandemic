{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Interpreter where

import Control.Monad.Writer.Strict

import Action
import Exception
import Notification
import PlayerCard
import Target

class Monad m => Interpreter m where
    showTarget :: Target -> m ()
    getAction :: Target -> m Action
    getCard :: Target -> m PlayerCard
    notifyAll :: [Notification] -> m ()
    endGame :: Target -> Loseable -> m (Target, Loseable)

runNotifications :: Interpreter m => WriterT [Notification] m a -> m a
runNotifications (WriterT mx) = do
  (x, ns) <- mx
  notifyAll ns
  return x

instance (MonadTrans t, Monad (t m), Interpreter m) => Interpreter (t m) where
  showTarget = lift . showTarget
  getAction = lift . getAction
  getCard = lift . getCard
  notifyAll = lift . notifyAll
  endGame = (lift .) . endGame
