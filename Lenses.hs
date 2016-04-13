{-# LANGUAGE TemplateHaskell #-}

module Lenses where

import           Control.Lens
import           Cures
import           Deck
import           Diseases
import           Globals
import           Player

makeLenses ''Cures
makeLenses ''Deck
makeLenses ''Diseases
makeLenses ''Globals
makeLenses ''Player
