module Action where

import City
import PlayerCard

data Action
  = Drive City
  | DirectFlight City
  | CharterFlight City
  | ShuttleFlight City
  | Build
  | Treat
  | GiveCard PlayerCard
  | TakeCard PlayerCard
  | DiscoverCure [PlayerCard]
  deriving (Show, Read)

