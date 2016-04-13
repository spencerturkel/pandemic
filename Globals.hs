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
import Lenses
import           InfectionCard
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
            , _infectionDeck         :: Deck InfectionCard
            , _infectionDiscard      :: Deck InfectionCard
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
    initial = Globals { _spaces = Map.empty -- TODO
                      , _players = p
                      , _playerLocations = Map.empty -- TODO All in Atlanta
                      , _infectionRateCounter = minBound
                      , _outbreakCounter = minBound
                      , _cures = Cures Uncured Uncured Uncured Uncured
                      , _diseaseSupply = Diseases 24 24 24 24
                      , _researchStationSupply = 5 -- TODO One in Atlanta
                      , _infectionDeck = undefined -- TODO
                      , _infectionDiscard = undefined -- TODO
                      , _playerDeck = undefined -- TODO
                      , _playerDiscard = undefined -- TODO
                      , _generator = g
                      , _eventEffects = []
                      }
    -- shuffle _infectionDeck
    -- do initial infections, draw 3 and put 3 on each, draw 3 and put 2 on each, draw 3 and put 1 on each
    -- shuffle _playerDeck without epidemics
    -- deal according to number of players, 2 -> 4, 3 -> 3, 4 -> 2
    -- split _playerDeck, insert epidemics according to config, and restack
  in
    initial -- TODO

shuffleInfectionDeck :: (MonadState g m, RandomGen g) => m Globals
shuffleInfectionDeck = do
  undefined
