{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE Rank2Types #-}

module Target where

import Control.Lens

import Globals
import Player

type PlayerRef = Int

type Target = (Globals, PlayerRef)

-- Functor f => (a -> f a) -> s -> f s
-- Target -> Player
-- Target -> Player -> Target

playerLens :: Lens' Target Player
playerLens = lens getter setter
  where
    getter (g, i) = head (g^..players.ix i)
    setter t@(_, i) p = t & _1.players.ix i .~ p
    --setter (t, i) p = t & _1.ix i .~ p
