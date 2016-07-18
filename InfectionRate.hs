{-# LANGUAGE DeriveGeneric #-}

module InfectionRate where

import Data.Aeson
import GHC.Generics

data InfectionRateCounter
  = RateTwoOne
  | RateTwoTwo
  | RateTwoThree
  | RateThreeOne
  | RateThreeTwo
  | RateFourOne
  | RateFourTwo
  deriving (Show, Read, Enum, Bounded, Generic)

instance ToJSON InfectionRateCounter

data InfectionRate
  = RateTwo
  | RateThree
  | RateFour
  deriving (Show, Read, Enum, Bounded, Generic)

instance ToJSON InfectionRate

getRate :: InfectionRateCounter -> InfectionRate
getRate RateTwoOne = RateTwo
getRate RateTwoTwo = RateTwo
getRate RateTwoThree = RateTwo
getRate RateThreeOne = RateThree
getRate RateThreeTwo = RateThree
getRate RateFourOne = RateFour
getRate RateFourTwo = RateFour
