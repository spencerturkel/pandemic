{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Diseases where

import Control.Lens
import Data.Aeson
import GHC.Generics

data DiseaseColor
  = Black
  | Blue
  | Red
  | Yellow
  deriving (Show, Read, Eq, Ord, Bounded, Enum, Generic)

instance FromJSON DiseaseColor
instance ToJSON DiseaseColor

diseasesAmount :: DiseaseColor -> Int
diseasesAmount = const 24

data Diseases
  = Diseases { _blackCubes :: Int
             , _blueCubes :: Int
             , _redCubes :: Int
             , _yellowCubes :: Int
             }
  deriving (Show, Read, Eq, Generic)
makeLenses ''Diseases

instance FromJSON Diseases
instance ToJSON Diseases

diseasesOfColor :: DiseaseColor -> Lens' Diseases Int
diseasesOfColor Black = blackCubes
diseasesOfColor Blue = blueCubes
diseasesOfColor Red = redCubes
diseasesOfColor Yellow = yellowCubes

addDisease :: DiseaseColor -> Diseases -> Diseases
addDisease Black = blackCubes +~ 1
addDisease Blue = blueCubes +~ 1
addDisease Red = redCubes +~ 1
addDisease Yellow = yellowCubes +~ 1

removeDisease :: DiseaseColor -> Diseases -> Diseases
removeDisease Black = blackCubes +~ 1
removeDisease Blue = blueCubes +~ 1
removeDisease Red = redCubes +~ 1
removeDisease Yellow = yellowCubes +~ 1

availableDiseases :: Diseases -> Bool
availableDiseases d = 0 `notElem` [ d^.blackCubes
                                 , d^.blueCubes
                                 , d^.redCubes
                                 , d^.yellowCubes ]
