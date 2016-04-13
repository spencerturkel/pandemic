module Player where

import           PlayerCard
import           Role

data Player
  = Player { _playerNumber :: Int
           , _playerHand   :: [PlayerCard]
           , _role         :: Role
           }
    deriving (Show, Read, Eq, Ord)
