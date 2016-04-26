{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Target where

import Control.Lens

import Globals
import Player

type PlayerRef = Lens' Globals Player

type Target = (Globals, Lens' Globals Player)
