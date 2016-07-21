{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE Rank2Types         #-}
{-# LANGUAGE TemplateHaskell    #-}

module CoAction where

import           Control.Comonad.Cofree
import           Control.Lens
import           Control.Monad
import           Cures
import           Data.List

import           City
import           Diseases
import           Globals
import           Player
import           PlayerCard
import           Space
import           Target

 -- Using a CoFree interpreter, see Dave Laing's blog

data CoActionF k
  = CoActionF { _driveH         :: City -> Maybe k
              , _directFlightH  :: City -> Maybe k
              , _charterFlightH :: City -> Maybe k
              , _shuttleFlightH :: City -> Maybe k
              , _buildH         :: City -> Maybe k
              , _treatH         :: DiseaseColor -> Maybe k
              , _giveCardH      :: PlayerRef -> PlayerCard -> Maybe k
              , _takeCardH      :: PlayerCard -> Maybe k
              , _discoverCureH  :: Lens' Player [City] -> Maybe k
              , _roleAbilityH   :: Ability -> Maybe k
              }
makeLenses ''CoActionF

instance Functor CoActionF where
  fmap f co = CoActionF { _driveH = fmap f <$> _driveH co
                        , _directFlightH = fmap f <$> _directFlightH co
                        , _charterFlightH = fmap f <$> _charterFlightH co
                        , _shuttleFlightH = fmap f <$> _shuttleFlightH co
                        , _buildH = fmap f <$> _buildH co
                        , _treatH = fmap f <$> _treatH co
                        , _giveCardH = fmap (fmap f) <$> _giveCardH co
                        , _takeCardH = fmap f <$> _takeCardH co
                        , _discoverCureH = fmap f <$> _discoverCureH co
                        , _roleAbilityH = fmap f <$> _roleAbilityH co
                        }

type CoAction a = Cofree CoActionF a

mkCoAction :: Target -> CoAction Target
mkCoAction = coiter go
  where
    go :: Target -> CoActionF Target
    go w = CoActionF { _driveH = coDrive w
                     , _directFlightH  = coDirectFlight w
                     , _charterFlightH = coCharterFlight w
                     , _shuttleFlightH = coShuttleFlight w
                     , _buildH = coBuild w
                     , _treatH = coTreat w
                     , _giveCardH = coGiveCard w
                     , _takeCardH = coTakeCard w
                     , _discoverCureH = coDiscoverCure w
                     , _roleAbilityH = coRoleAbility w
                     }

coDrive :: Target -> City -> Maybe Target
coDrive target destination =
    if citiesConnected destination $ target^.playerLens.location then
      Just $ target & playerLens.location .~ destination
    else
      Nothing

coDirectFlight :: Target -> City -> Maybe Target
coDirectFlight target destination =
  let
    card = PlayerCard destination
  in
    if card `elem` target^.playerLens.playerHand then
      Just $ target &~ do
        playerLens.location .= destination
        playerLens.playerHand %= delete card
    else
      Nothing

coCharterFlight :: Target -> City -> Maybe Target
coCharterFlight target destination =
    if target^.playerLens.location.to PlayerCard `elem`
       target^.playerLens.playerHand then
      Just $ target &~ do
        playerLens.location .= destination
        playerLens.playerHand %= delete (PlayerCard destination)
    else
      Nothing

coShuttleFlight :: Target -> City -> Maybe Target
coShuttleFlight target destination =
  let
    researchAtLocation  = target^.playerSpace.hasResearchStation
    researchAtDestination = target^._1.spaceAtCity destination.hasResearchStation
  in
    if researchAtDestination && researchAtLocation then
      Just $ target & playerLens.location .~ destination
    else
      Nothing

coBuild :: Target -> City -> Maybe Target
coBuild target fromCity =
  let
    supplyEmpty = target^._1.researchStationSupply == 1
    playerLocation = target^.playerLens.location
    playerLocationCard = PlayerCard playerLocation
  in
    if playerLocationCard `elem` target^.playerLens.playerHand then
      Just $ target &~ do
        _1.spaceAtCity playerLocation.hasResearchStation .= True
        playerLens.playerHand %= delete playerLocationCard
        when supplyEmpty $
          _1.spaceAtCity fromCity.hasResearchStation .= False
    else
      Nothing

coTreat :: Target -> DiseaseColor -> Maybe Target
coTreat target color =
  let
    count = target^.playerSpace.diseases.diseasesOfColor color
  in
    if count > 0 then
      Just $ target &~
      if Cured == target^._1.cures.cureStatus color then do
        playerSpace.diseases.diseasesOfColor color .= 0
        replicateM_ count $ _1.diseaseSupply %= addDisease color
        when (target^._1.diseaseSupply.diseasesOfColor color
              == diseasesAmount color) $
          _1.cures.cureStatus color .= Eradicated
      else do
        playerSpace.diseases %= removeDisease color
        _1.diseaseSupply %= addDisease color
    else
      Nothing

coGiveCard :: Target -> PlayerRef -> PlayerCard -> Maybe Target
coGiveCard target ref card =
  let
    refLens :: Lens' Target Player
    refLens = lens getter setter
      where
        getter t = head (t^.._1.players.ix ref)
        setter t p = t & _1.players.ix ref .~ p

    refHandSize = target^.refLens.playerHand.to length
    refLocation = target^.refLens.location

    playerLocation = target^.playerLens.location
    playerLocationCard = PlayerCard playerLocation
    playerHasCard = playerLocationCard `elem` target^.playerLens.playerHand
  in
    if playerLocation == refLocation && playerHasCard then
      Just $ target &~ do
      playerLens.playerHand %= delete playerLocationCard
      refLens.playerHand %= (playerLocationCard:)
      when (refHandSize + 1 > handLimit) $
        refLens.playerHand %= delete card
    else
      Nothing

coTakeCard :: Target -> PlayerCard -> Maybe Target
coTakeCard target card =
  let
    locationCard = PlayerCard $ target^.playerLens.location
  in
    case find (\p -> locationCard `elem` p^.playerHand)
    (target^._1.players) of
      Nothing -> Nothing
      Just other ->
        Just $ target &~ do
        playerLens.playerHand %= (locationCard :)
        _1.players.traversed.filtered (== other).playerHand %= delete locationCard
        when (target^.playerLens.playerHand.to length > handLimit) $
            playerLens.playerHand %= delete card

coDiscoverCure :: Target -> Lens' Player [City] -> Maybe Target
coDiscoverCure target ref =
  let
    cards = target^.playerLens.ref
  in
    case cards of
      x : xs@[_, _, _, _] ->
        let
          color = colorOfCity x
        in
          if all (== color) $ map colorOfCity xs then
            Just $ target &~ do
              playerLens.playerHand %= (\\ map PlayerCard cards)
              when (target^._1.diseaseSupply.diseasesOfColor color
                 == diseasesAmount color) $
                _1.cures.cureStatus color .= Eradicated
          else
            Nothing
      _ -> Nothing

coRoleAbility :: Target -> Ability -> Maybe Target
coRoleAbility _ _ = Nothing -- TODO
