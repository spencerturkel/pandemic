module OutbreakCounter where

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
  deriving (Show, Read, Eq, Ord, Enum, Bounded)
