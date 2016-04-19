{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Cures where

import           Control.Lens

import Diseases

data CureStatus
  = Uncured
  | Cured
  | Eradicated
  deriving (Show, Read, Eq)

data Cures
  = Cures { _redCure :: CureStatus
          , _yellowCure :: CureStatus
          , _blueCure :: CureStatus
          , _blackCure :: CureStatus
          }
  deriving (Show, Read)
makeLenses ''Cures

cureStatus :: DiseaseColor -> Lens' Cures CureStatus
cureStatus Red = redCure
cureStatus Yellow = yellowCure
cureStatus Blue = blueCure
cureStatus Black = blackCure
