{-# LANGUAGE TemplateHaskell #-}

module Space where

import           Control.Lens

import           City
import           Diseases

data Space
  = Space { _city               :: City
          , _hasResearchStation :: Bool
          , _diseases           :: Diseases
          } deriving (Show, Read)
makeLenses ''Space
