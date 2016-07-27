{-# LANGUAGE ConstraintKinds #-}

module Notification where

import Control.Monad.Writer.Strict

data Notification
  = N1
  | N2
  deriving (Show, Read)

type NotificationWriter = MonadWriter [Notification]
