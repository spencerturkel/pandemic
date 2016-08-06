{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Notification where

import Control.Monad.Writer.Strict

import PlayerCard
import City

data Notification
  = Infecting City
  | OutbreakIn City
  | EpidemicIn City
  | Drew PlayerCard
  deriving (Show, Read)

type NotificationWriter = MonadWriter [Notification]

notify :: NotificationWriter m => Notification -> m ()
notify x = tell [x]
