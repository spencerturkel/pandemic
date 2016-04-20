{-# LANGUAGE TemplateHaskell #-}

module Player where

import Control.Lens

import           PlayerCard

data Role
  = ContingencyPlanner (Maybe PlayerCard)
  | Dispatcher
  | Medic
  | OperationsExpert
  | QuarantineSpecialist
  | Researcher
  | Scientist
  deriving (Show, Read, Eq, Ord)

data Player
  = Player { _playerNumber :: Int
           , _playerHand   :: [PlayerCard]
           , _role         :: Role
           }
    deriving (Show, Read, Eq, Ord)
makeLenses ''Player

handLimit :: Int
handLimit = 7
