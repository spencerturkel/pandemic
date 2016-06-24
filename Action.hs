{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RankNTypes         #-}
module Action where

import           Control.Lens

import           City
import           Diseases
import           Player
import           PlayerCard
import           Target

data Action
  = Drive City
  | DirectFlight City
  | CharterFlight City
  | ShuttleFlight City
  | Build City
  | Treat DiseaseColor
  | GiveCard PlayerRef PlayerCard
  | TakeCard PlayerCard
  | DiscoverCure (Lens' Player [City])
  | RoleAbility Ability
