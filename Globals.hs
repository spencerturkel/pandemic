module Globals where

import           Cures
import           Diseases
import           InfectionRate

data Globals
  = Globals { _infectionRateCounter :: InfectionRateCounter
            , _outbreakCounter      :: OutbreakCounter
            , _cures                :: Cures
            , _diseases             :: Diseases
            , _researchStations     :: Int
            }
    deriving (Show, Read)

data OutbreakCounter
  = OutbreakOne
  | OutbreakTwo
  | OutbreakThree
  | OutbreakFour
  | OutbreakFive
  | OutbreakSix
  | OutbreakSeven
  | OutbreakEight
  deriving (Show, Read, Enum, Bounded)

data PlayerTurn
  = Action1
  | Action2
  | Action3
  | Action4
  | Draw1
  | Draw2
  | Infect
