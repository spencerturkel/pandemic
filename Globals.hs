module Globals where

import           System.Random

import           Cures
import           Deck
import           Diseases
import           InfectionCard
import           InfectionRate
import           PlayerCard

data Globals
  = Globals { _infectionRateCounter :: InfectionRateCounter
            , _outbreakCounter      :: OutbreakCounter
            , _cures                :: Cures
            , _diseases             :: Diseases
            , _researchStations     :: Int
            , _infectionDeck        :: Deck InfectionCard
            , _infectionDiscard     :: Deck InfectionCard
            , _playerDeck           :: Deck PlayerCard
            , _playerDiscard        :: Deck PlayerCard
            , _generator            :: StdGen
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
