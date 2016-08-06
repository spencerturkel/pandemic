{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Space where

import           Control.Lens
import           Data.Aeson
import           GHC.Generics

import           City
import           Diseases

data Space
  = Space { _city               :: City
          , _hasResearchStation :: Bool
          , _diseases           :: Diseases
          } deriving (Show, Read, Generic)
makeLenses ''Space

instance ToJSON Space
instance FromJSON Space
