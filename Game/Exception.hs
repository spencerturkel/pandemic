module Exception where

import Diseases(DiseaseColor(..))

data Exception
  = DrawFromEmptyDiseasePile
  | DrawFromEmptyInfectionDeck
  | DrawFromEmptyPlayerDeck
  deriving (Show, Read)

data Loseable
  = PlayerDeckExhausted
  | EigthOutbreak
  | DiseaseExhausted DiseaseColor
  deriving (Show, Read)
