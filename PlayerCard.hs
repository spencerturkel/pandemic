module PlayerCard where

import City

data PlayerCard
  = PlayerCard City
  | Event
  deriving (Show, Read)
