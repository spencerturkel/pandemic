module Exception where

data Exception
  = DrawFromEmptyDeck
  | DrawFromEmptyPlayerDeck
  | DrawFromEmptyInfectionDeck
  | DrawFromEmptyDiseasePile
  deriving (Show, Read)
