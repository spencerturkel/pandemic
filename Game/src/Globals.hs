{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE TemplateHaskell #-}

module Globals where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.State
import           Data.List
import           Data.Maybe
import GHC.Generics(Generic)
import           System.Random

import           City
import           Cures
import           Deck
import           Diseases
import Exception
import           InfectionRate
import           Notification
import           OutbreakCounter
import           Player
import           PlayerCard
import           Space

data Globals
  = Globals { _spaces                :: [Space]
            , _players               :: [Player]
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
            }
    deriving (Show, Read, Generic)
makeLenses ''Globals

spaceAtCity :: City -> Lens' Globals Space
spaceAtCity theCity = lens getter setter
  where
    getter :: Globals -> Space
    getter Globals { _spaces = xs } =
      fromMaybe (error "City not found in spaces. This should never happen.") $
      find (\Space { _city = c } -> c == theCity) xs

    setter :: Globals -> Space -> Globals
    setter g@Globals { _spaces = [] } s = g & spaces .~ [s]
    setter g@Globals { _spaces = (x:xs) } s = g &~
      if x^.city == theCity then
        spaces .= s:xs
      else do
        spaces .= xs
        modify $ flip setter s
        spaces %= (x:)

primInfect ::
  (MonadError Loseable m, MonadState Globals m, NotificationWriter m) =>
  City -> DiseaseColor -> (City -> m ()) -> m ()
primInfect thisCity color f = do
  notify $ Infecting thisCity
  status <- use $ cures.cureStatus color
  ([space], otherSpaces) <- use $ spaces.to (partition ((thisCity ==) . _city))
  let diseaseCount = space^.diseases.diseasesOfColor color
  unless (status == Eradicated) $
    if diseaseCount == 3 then
      f thisCity
      else do
      spaces .= (space & diseases %~ addDisease color) : otherSpaces
      diseaseSupply %= removeDisease color
      supply <- use diseaseSupply
      unless (availableDiseases supply) $
        throwError $ DiseaseExhausted color

infect ::
  (MonadError Loseable m, MonadState Globals m, NotificationWriter m) =>
  City -> DiseaseColor -> m ()
infect cityToInfect color = primInfect cityToInfect color doOutbreak

doInfectionStep :: (MonadError Loseable m, MonadState Globals m, NotificationWriter m) => m ()
doInfectionStep = do
  count <- fromEnum <$> use infectionRateCounter
  replicateM_ count doNextInfection

doNextInfection :: (MonadError Loseable m, MonadState Globals m, NotificationWriter m) => m ()
doNextInfection = do
  Right cityToInfect <-
    runExceptT (drawFrom infectionDeck
                :: MonadState Globals m => ExceptT DeckException m City)
  infect cityToInfect $ colorOfCity cityToInfect
  infectionDiscard %= addToDeck cityToInfect

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

doOutbreak :: (MonadError Loseable m, MonadState Globals m, NotificationWriter m)
  => City -> m ()
doOutbreak c = do
  notify $ OutbreakIn c
  go [c] c
    where
      go cities thisCity = do
        count <- outbreakCounter <%= succ
        when (count == maxBound) $ throwError EigthOutbreak
        (\f ->foldM_ f cities $ connectionsFromCity thisCity)
          $ \visited thisLocation ->
          if thisLocation `elem` visited then
            return visited
          else do
            let visited' = thisLocation:visited
            primInfect thisLocation (colorOfCity thisCity) (go visited')
            return visited'
