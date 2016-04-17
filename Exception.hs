module Exception where

data Exception
  = DrawFromEmptyDiseasePile
  | DrawFromEmptyInfectionDeck
  | DrawFromEmptyPlayerDeck
  deriving (Show, Read)
