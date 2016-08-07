{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Player where

import           Control.Lens
import           Data.Aeson
import           GHC.Generics

import           City
import           PlayerCard

data Role
  = ContingencyPlanner (Maybe PlayerCard)
  | Dispatcher
  | Medic
  | OperationsExpert
  | QuarantineSpecialist
  | Researcher
  | Scientist
  deriving (Show, Read, Eq, Ord, Generic)

instance ToJSON Role
instance FromJSON Role

data Ability
  = ContingencyPlannerEvent PlayerCard
  | DispatcherMoveOtherPawnToCityWithAnotherPawn Player Player
  | DispatcherMoveAnotherPawnAsIfOwn Player City
  | OperationsExpertBuildResearch
  | OperationsExpertMoveFromResearch City
  deriving (Show, Read)

data Player
  = Player { _playerNumber :: Int
           , _playerHand   :: [PlayerCard]
           , _role         :: Role
           , _location         :: City
           }
    deriving (Show, Read, Eq, Ord, Generic)
makeLenses ''Player

instance ToJSON Player
instance FromJSON Player

handLimit :: Int
handLimit = 7
