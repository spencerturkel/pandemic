{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

module Cures where

import           Control.Lens
import           Data.Aeson
import           GHC.Generics

import           Diseases

data CureStatus
  = Uncured
  | Cured
  | Eradicated
  deriving (Show, Read, Eq, Generic)

instance ToJSON CureStatus
instance FromJSON CureStatus

data Cures
  = Cures { _redCure    :: CureStatus
          , _yellowCure :: CureStatus
          , _blueCure   :: CureStatus
          , _blackCure  :: CureStatus
          }
  deriving (Show, Read, Generic)
makeLenses ''Cures

allCured :: Cures -> Bool
allCured (Cures red yellow blue black) = not $ any (== Uncured) [red, yellow, blue, black]

instance ToJSON Cures
instance FromJSON Cures

cureStatus :: DiseaseColor -> Lens' Cures CureStatus
cureStatus Red = redCure
cureStatus Yellow = yellowCure
cureStatus Blue = blueCure
cureStatus Black = blackCure
