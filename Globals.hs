{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE TemplateHaskell #-}

module Globals where

import           Control.Lens
import           Control.Monad.State
import           Control.Monad.Except
import           Data.Map.Lazy       (Map)
import qualified Data.Map.Lazy       as Map
import           System.Random

import           City
import           Cures
import           Deck
import           Diseases
import Exception
import           EventEffect
import           InfectionRate
import           OutbreakCounter
import           Player
import           PlayerCard

data Globals
  = Globals { _spaces                :: Map City Diseases
            , _researchLocations     :: Map City Bool
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
                      , _researchLocations = Map.fromList $
                        map ((\(c,b) -> if c == Atlanta then (c, True) else (c,b))
                        . flip (,) False) [minBound..maxBound]
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
  in initial &~ do
    modify $ shuffleDeck infectionDeck
    modify $ shuffleDeck playerDeck
    undefined -- TODO do initial infections
    Right hands <-
      runExceptT . replicateM (length p) . flip replicateM (drawFrom playerDeck)
      $ case compare (length p) 3 of LT -> 4
                                     EQ -> 3
                                     GT -> 2
    players %= zipWith (set playerHand) hands
    undefined -- TODO split _playerDeck, insert epidemics according to config, and restack

drawFrom ::
  (MonadError DeckException m, MonadState Globals m) =>
  Lens' Globals (Deck a) -> m a
drawFrom target = do
    deck <- use target
    (card, deck') <- runStateT drawFromDeck deck
    target .= deck'
    return card

shuffleDeck :: Lens' Globals (Deck a) -> Globals -> Globals
shuffleDeck deck global =
  let
    (d, g) = runState (shuffle (global^.deck)) (global^.generator)
  in
    global & deck .~ d & generator .~ g

    -- do initial infections, draw 3 and put 3 on each, draw 3 and put 2 on each, draw 3 and put 1 on each
doInitialInfections = undefined

infect ::
  (MonadError Exception m, MonadState Globals m) =>
  City -> DiseaseColor -> m ()
infect city color = do
      spaces %= Map.adjust (addDisease color) city
      diseaseSupply %= removeDisease color
      supply <- use diseaseSupply
      unless (availableDiseases supply) $
        throwError DrawFromEmptyDiseasePile
