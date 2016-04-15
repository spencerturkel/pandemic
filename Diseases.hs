{-# LANGUAGE TemplateHaskell #-}

module Diseases where

import Control.Lens

data DiseaseColor
  = Black
  | Blue
  | Red
  | Yellow
  deriving (Show, Read, Eq, Ord, Bounded, Enum)

data Diseases
  = Diseases { _blackCubes :: Int
             , _blueCubes :: Int
             , _redCubes :: Int
             , _yellowCubes :: Int
             }
  deriving (Show, Read, Eq)
makeLenses ''Diseases

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
