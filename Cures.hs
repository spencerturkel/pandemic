{-# LANGUAGE TemplateHaskell #-}

module Cures where

import           Control.Lens

data CureStatus
  = Uncured
  | Cured
  | Eradicated
  deriving (Show, Read)

data Cures
  = Cures { _redCure :: CureStatus
          , _yellowCure :: CureStatus
          , _blueCure :: CureStatus
          , _blackCure :: CureStatus
          }
  deriving (Show, Read)
makeLenses ''Cures
