{-# LANGUAGE TemplateHaskell #-}

module Diseases where

import Control.Lens

data Diseases
  = Diseases { _blackCubes :: Int
             , _blueCubes :: Int
             , _redCubes :: Int
             , _yellowCubes :: Int
             }
  deriving (Show, Read, Eq)
makeLenses ''Diseases

