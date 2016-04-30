{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Target where

import Control.Lens

import Globals
import Player

type PlayerRef = Int

type Target = (Globals, Int)
