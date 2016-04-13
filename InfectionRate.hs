module InfectionRate where

data InfectionRateCounter
  = RateTwoOne
  | RateTwoTwo
  | RateTwoThree
  | RateThreeOne
  | RateThreeTwo
  | RateFourOne
  | RateFourTwo
  deriving (Show, Read, Enum, Bounded)

data InfectionRate
  = RateTwo
  | RateThree
  | RateFour
  deriving (Show, Read, Enum, Bounded)

getRate :: InfectionRateCounter -> InfectionRate
getRate RateTwoOne = RateTwo
getRate RateTwoTwo = RateTwo
getRate RateTwoThree = RateTwo
getRate RateThreeOne = RateThree
getRate RateThreeTwo = RateThree
getRate RateFourOne = RateFour
getRate RateFourTwo = RateFour
