{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RankNTypes         #-}
module Action where

import           Control.Lens
import           Control.Monad.Trans.Free

import           City
import           Diseases
import           Globals
import           Player
import           PlayerCard

type PlayerRef = Lens' Globals Player

data ActionF k
  = Drive City (Bool -> k)
  | DirectFlight City (Bool -> k)
  | CharterFlight City (Bool -> k)
  | ShuttleFlight City (Bool -> k)
  | Build City (Bool -> k)
  | Treat DiseaseColor (Bool -> k)
  | GiveCard PlayerRef (Bool -> k)
  | TakeCard PlayerCard (Bool -> k)
  | DiscoverCure (Lens' Player [City]) (Bool -> k)
  | RoleAbility Ability (Bool -> k)
  deriving (Functor)

type ActionT m a = FreeT ActionF m a

--interpret :: Monad m => Globals -> Player -> ActionT m r -> m r
--interpret globals player action = do
--  act <- action
--  case act of
--    Pure x -> return x
--    Free (Drive city f) ->
--      let
--        canReach = citiesConnected city $ globals ^?! playerLocations.at player
--        newGlobals = if canReach then
--                       globals ^. playerLocations. at player .~ city
--                     else
--                       globals
--      in
--        interpret $ f canReach

drive :: Monad m => City -> ActionT m Bool
drive city = liftF $ Drive city id

directFlight :: Monad m => City -> ActionT m Bool
directFlight city = liftF $ DirectFlight city id

charterFlight :: Monad m => City -> ActionT m Bool
charterFlight city = liftF $ CharterFlight city id

shuttleFlight :: Monad m => City -> ActionT m Bool
shuttleFlight city = liftF $ ShuttleFlight city id

build :: Monad m => City -> ActionT m Bool
build city = liftF $ Build city id

treat :: Monad m => DiseaseColor -> ActionT m Bool
treat color = liftF $ Treat color id

giveCard :: Monad m => PlayerRef -> ActionT m Bool
giveCard ref = liftF $ GiveCard ref id

takeCard :: Monad m => PlayerCard -> ActionT m Bool
takeCard c = liftF $ TakeCard c id

discoverCure :: Monad m => Lens' Player [City] -> ActionT m Bool
discoverCure ref = liftF $ DiscoverCure ref id

roleAbility :: Monad m => Ability -> ActionT m Bool
roleAbility ability = liftF $ RoleAbility ability id

