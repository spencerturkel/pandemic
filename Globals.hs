{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE TemplateHaskell #-}

module Globals where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.State
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

infect ::
  (MonadError Loseable m, MonadState Globals m) =>
  City -> DiseaseColor -> m ()
infect city color = do
  status <- use (cures.cureStatus color)
  count <- use (spaces.at city.non undefined.diseasesOfColor color)
  unless (status == Eradicated) $
    if count == 3 then
      doOutbreak city
      else do
      spaces %= Map.adjust (addDisease color) city
      diseaseSupply %= removeDisease color
      supply <- use diseaseSupply
      unless (availableDiseases supply) $
        throwError $ DiseaseExhausted color

doNextInfection :: (MonadError Loseable m, MonadState Globals m) => m ()
doNextInfection = do
  Right city <-
    runExceptT (drawFrom infectionDeck
                :: MonadState Globals m => ExceptT DeckException m City)
  infect city $ colorOfCity city

shuffleDeck :: Lens' Globals (Deck a) -> Globals -> Globals
shuffleDeck deck global =
  let
    (d, g) = runState (shuffle (global^.deck)) (global^.generator)
  in
    global & deck .~ d & generator .~ g

drawFrom ::
  (MonadError DeckException m, MonadState Globals m) =>
  Lens' Globals (Deck a) -> m a
drawFrom target = do
    deck <- use target
    (card, deck') <- runStateT drawFromDeck deck
    target .= deck'
    return card

doEvent :: EventEffect -> Globals -> Globals
doEvent = undefined -- TODO

doOutbreak :: (MonadError Loseable m, MonadState Globals m) => City -> m ()
doOutbreak city = go Map.empty city
  where
    go ::
      (MonadError Loseable m, MonadState Globals m)
      => Map City Bool -> City -> m ()
    go map city = undefined
