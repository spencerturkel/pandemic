{-# LANGUAGE TemplateHaskell #-}

module Player where

import Control.Lens

import           PlayerCard
import           Role

data Player
  = Player { _playerNumber :: Int
           , _playerHand   :: [PlayerCard]
           , _role         :: Role
           }
    deriving (Show, Read, Eq, Ord)
makeLenses ''Player
