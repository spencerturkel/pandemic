{-# LANGUAGE TemplateHaskell #-}

module Lenses where

import           Control.Lens
import           Cures
import           Deck
import           Diseases
import           Globals

makeLenses ''Cures
makeLenses ''Deck
makeLenses ''Globals
makeLenses ''Diseases
