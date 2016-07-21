{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE Rank2Types #-}

module Target where

import Control.Lens

import Globals
import Player
import Space

type PlayerRef = Int

type Target = (Globals, PlayerRef)

playerLens :: Lens' Target Player
playerLens = lens getter setter
  where
    getter (g, i) = head (g^..players.ix i)
    setter t@(_, i) p = t & _1.players.ix i .~ p

playerSpace :: Lens' Target Space
playerSpace = lens getter setter
  where
    getter target = target^._1.spaceAtCity (target^.playerLens.location)
    setter target space = target
      & _1.spaceAtCity (target^.playerLens.location) .~ space
