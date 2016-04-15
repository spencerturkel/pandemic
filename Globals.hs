{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}

module Globals where

import Control.Monad.State
import Control.Lens
import           Data.Map.Lazy   (Map)
import qualified Data.Map.Lazy   as Map
import           System.Random

import           City
import           Cures
import           Deck
import           Diseases
import           EventEffect
import           InfectionRate
import           OutbreakCounter
import           Player
import           PlayerCard

data Globals
  = Globals { _spaces                :: Map City Diseases
            , _players               :: [Player]
            , _playerLocations       :: Map Player City
            , _infectionRateCounter  :: InfectionRateCounter
            , _outbreakCounter       :: OutbreakCounter
            , _cures                 :: Cures
            , _diseaseSupply         :: Diseases
            , _researchStationSupply :: Int
            , _infectionDeck         :: Deck City
            , _infectionDiscard      :: Deck City
            , _playerDeck            :: Deck PlayerCard
            , _playerDiscard         :: Deck PlayerCard
            , _generator             :: StdGen
            , _eventEffects          :: [EventEffect]
            }
    deriving (Show, Read)
makeLenses ''Globals

makeGlobals :: StdGen -> [Player] -> Globals
makeGlobals g p =
  let
    initial = Globals { _spaces = Map.fromList $
                        map (flip (,) (Diseases 0 0 0 0)) [minBound..maxBound]
                      , _players = p
                      , _playerLocations = Map.fromList $
                        map (flip (,) Atlanta) p
                      , _infectionRateCounter = minBound
                      , _outbreakCounter = minBound
                      , _cures = Cures Uncured Uncured Uncured Uncured
                      , _diseaseSupply = Diseases 24 24 24 24
                      , _researchStationSupply = 5
                      , _infectionDeck = Deck [minBound..maxBound]
                      , _infectionDiscard = Deck []
                      , _playerDeck = Deck $ map PlayerCard [minBound..maxBound]
                      , _playerDiscard = Deck []
                      , _generator = g
                      , _eventEffects = []
                      }
  in flip execState initial $ do
    modify $ shuffleDeck infectionDeck
    -- do initial infections, draw 3 and put 3 on each, draw 3 and put 2 on each, draw 3 and put 1 on each
    -- shuffle _playerDeck without epidemics
    modify $ shuffleDeck playerDeck
    -- deal according to number of players, 2 -> 4, 3 -> 3, 4 -> 2
    -- set playerLocations to Atlanta
    -- put research in Atlanta
    -- split _playerDeck, insert epidemics according to config, and restack

shuffleDeck :: Lens' Globals (Deck a) -> Globals -> Globals
shuffleDeck deck global =
  let
    (d, g) = runState (shuffle (global^.deck)) (global^.generator)
  in
    global & deck .~ d & generator .~ g

--doInitialInfections :: 
