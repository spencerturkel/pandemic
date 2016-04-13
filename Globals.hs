module Globals where

import           System.Random

import           Cures
import           Deck
import           Diseases
import           InfectionCard
import           InfectionRate
import           PlayerCard

data Globals
  = Globals { _infectionRateCounter  :: InfectionRateCounter
            , _outbreakCounter       :: OutbreakCounter
            , _cures                 :: Cures
            , _diseaseSupply         :: Diseases
            , _researchStationSupply :: Int
            , _infectionDeck         :: Deck InfectionCard
            , _infectionDiscard      :: Deck InfectionCard
            , _playerDeck            :: Deck PlayerCard
            , _playerDiscard         :: Deck PlayerCard
            , _generator             :: StdGen
            }
    deriving (Show, Read)

makeGlobals :: StdGen -> Globals
makeGlobals g =
  let
    initial = Globals { _infectionRateCounter = minBound
                      , _outbreakCounter = minBound
                      , _cures = Cures Uncured Uncured Uncured Uncured
                      , _diseaseSupply = Diseases 24 24 24 24
                      , _researchStationSupply = 5 -- TODO One in Atlanta
                      , _infectionDeck = undefined -- TODO
                      , _infectionDiscard = undefined -- TODO
                      , _playerDeck = undefined -- TODO
                      , _playerDiscard = undefined -- TODO
                      , _generator = g
                      }
    -- shuffle _infectionDeck
    -- do initial infections, draw 3 and put 3 on each, draw 3 and put 2 on each, draw 3 and put 1 on each
    -- shuffle _playerDeck without epidemics
    -- deal according to number of players, 2 -> 4, 3 -> 3, 4 -> 2
    -- split _playerDeck, insert epidemics according to config, and restack
  in
    initial -- TODO

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
  deriving (Show, Read, Enum, Bounded)

data PlayerTurn
  = Action1
  | Action2
  | Action3
  | Action4
  | Draw1
  | Draw2
  | Infect
