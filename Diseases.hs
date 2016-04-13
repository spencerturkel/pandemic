module Diseases where

data Diseases
  = Diseases { _blackCubes :: Int
             , _blueCubes :: Int
             , _redCubes :: Int
             , _yellowCubes :: Int
             }
  deriving (Show, Read, Eq)

