module PlayerCard where

import City
import EventEffect

data PlayerCard
  = PlayerCard City
  | Event EventEffect
  | Epidemic
  deriving (Show, Read, Eq, Ord)

isPlayerCard :: PlayerCard -> Bool
isPlayerCard (PlayerCard _) = True
isPlayerCard _ = False
