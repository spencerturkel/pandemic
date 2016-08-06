{-# LANGUAGE DeriveGeneric #-}

module PlayerCard where

import Data.Aeson
import GHC.Generics

import City

data PlayerCard
  = PlayerCard City
  | Epidemic
  deriving (Show, Read, Eq, Ord, Generic)

instance FromJSON PlayerCard
instance ToJSON PlayerCard

isPlayerCard :: PlayerCard -> Bool
isPlayerCard (PlayerCard _) = True
isPlayerCard _ = False
