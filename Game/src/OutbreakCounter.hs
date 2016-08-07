{-# LANGUAGE DeriveGeneric #-}

module OutbreakCounter where

import Data.Aeson
import GHC.Generics

data OutbreakCounter
  = OutbreakZero
  | OutbreakOne
  | OutbreakTwo
  | OutbreakThree
  | OutbreakFour
  | OutbreakFive
  | OutbreakSix
  | OutbreakSeven
  | OutbreakEight
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

instance ToJSON OutbreakCounter
