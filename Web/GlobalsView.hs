{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types       #-}
{-# LANGUAGE TemplateHaskell  #-}

module GlobalsView where

import           Control.Lens
import           Data.Aeson
import           GHC.Generics    (Generic)

import           City
import           Cures
import           Deck
import           Diseases
import qualified Globals         as G
import           InfectionRate
import           OutbreakCounter
import           Player
import           PlayerCard
import           Space


data GlobalsView
  = GlobalsView { _spaces            :: [Space]
            , _players               :: [Player]
            , _infectionRateCounter  :: InfectionRateCounter
            , _outbreakCounter       :: OutbreakCounter
            , _cures                 :: Cures
            , _diseaseSupply         :: Diseases
            , _researchStationSupply :: Int
            , _infectionDiscard      :: Deck City
            , _playerDiscard         :: Deck PlayerCard
            }
    deriving (Show, Read, Generic)

instance ToJSON GlobalsView

viewGlobals :: G.Globals -> GlobalsView
viewGlobals g = case g of
  G.Globals { G._spaces = spaces
            , G._players = players
            , G._infectionRateCounter = infectionRateCounter
            , G._outbreakCounter = outbreakCounter
            , G._cures = cures
            , G._diseaseSupply = diseaseSupply
            , G._researchStationSupply = researchStationSupply
            , G._infectionDiscard = infectionDiscard
            , G._playerDiscard = playerDiscard
            } -> GlobalsView { _spaces = spaces
                             , _players               = players
                             , _infectionRateCounter  = infectionRateCounter
                             , _outbreakCounter       = outbreakCounter
                             , _cures                 = cures
                             , _diseaseSupply         = diseaseSupply
                             , _researchStationSupply = researchStationSupply
                             , _infectionDiscard      = infectionDiscard
                             , _playerDiscard         = playerDiscard
                             }

makeLenses ''GlobalsView
