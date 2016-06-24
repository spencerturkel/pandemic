{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE Rank2Types         #-}
{-# LANGUAGE TemplateHaskell    #-}

module CoAction where

import           Control.Applicative
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

(?%~) :: Functor f => ASetter s t (f a) (f b) -> (a -> b) -> s -> t
(?%~) mx f = mx %~ fmap f
infixr 4 ?%~

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
coDrive target@(globals, playerIx) city =
  let
    player = globals^.players.to (!! playerIx)
  in
    case citiesConnected city <$> globals^.playerLocations.at player of
      Just True -> Just $ target & _1.playerLocations.at player ?~ city
      _ -> Nothing

coDirectFlight :: Target -> City -> Maybe Target
coDirectFlight target@(globals, playerIx) city =
  let
    playerLens :: Lens' Globals Player
    playerLens = lens get setter
      where
        get g = g^.players.to (!! playerIx)
        setter g p = g & players.ix playerIx .~ p
    player = globals^.playerLens
    card = PlayerCard city
  in
    if card `elem` player^.playerHand then
      Just
      $ target
      & _1.playerLocations.at player ?~ city
      & _1.playerLens.playerHand %~ filter (/= card)
      else
      Nothing

coCharterFlight :: Target -> City -> Maybe Target
coCharterFlight target@(globals, playerIx) city =
  let
    playerLens :: Lens' Globals Player
    playerLens = lens get setter
      where
        get g = g^.players.to (!! playerIx)
        setter g p = g & players.ix playerIx .~ p
    player = globals^.playerLens
    Just location = PlayerCard <$> globals ^. playerLocations.at player
  in
    if location `elem` player^.playerHand then
      Just $ target
      & _1.playerLocations.at player ?~ city
      & _1.playerLens.playerHand %~ filter (/= location)
    else
      Nothing

coShuttleFlight :: Target -> City -> Maybe Target
coShuttleFlight target@(globals, playerIx) city =
  let
    player = globals^.players.to (!! playerIx)
    Just location = globals^.playerLocations.at player
    researchAtLocation = globals^.researchLocations.at location
    researchAtCity = globals^.researchLocations.at city
  in
    if Just True == liftA2 (&&) researchAtCity researchAtLocation then
      Just $ target & _1.playerLocations.at player ?~ city
    else
      Nothing

coBuild :: Target -> City -> Maybe Target
coBuild target@(globals, playerIx) city =
  let
    playerLens :: Lens' Globals Player
    playerLens = lens get setter
      where
        get g = g^.players.to (!! playerIx)
        setter g p = g & players.ix playerIx .~ p

    player = globals^.playerLens
    Just location = globals^.playerLocations.at player
    supplyEmpty = globals^.researchStationSupply == 1
  in
    if PlayerCard location `elem` player^.playerHand then
      let
        result = target
          & _1.researchLocations.at location ?~ True
          & _1.playerLens.playerHand %~ filter (/= PlayerCard location)
      in Just $
        if supplyEmpty then
          result & _1.researchLocations.at city ?~ False
        else
          result
    else
      Nothing

coTreat :: Target -> DiseaseColor -> Maybe Target
coTreat target@(globals, playerIx) color =
  let
    player = globals^.players.to (!! playerIx)
    Just location = globals^.playerLocations.at player
    Just diseases = globals^.spaces.at location
    count = diseases^.diseasesOfColor color
  in
    if count > 0 then
      if Cured == globals^.cures.cureStatus color then
        Just $ target
        & _1.spaces.at location ?%~ diseasesOfColor color .~ 0
        &~ replicateM_ count (_1.diseaseSupply %= addDisease color)
        & _1.cures.cureStatus color %~
        if globals^.diseaseSupply.diseasesOfColor color
           == diseasesAmount color then
          const Eradicated
        else
          id
      else
        Just $ target
        & _1.spaces.at location ?%~ removeDisease color
        & _1.diseaseSupply %~ addDisease color
    else
      Nothing

coGiveCard :: Target -> PlayerRef -> PlayerCard -> Maybe Target
coGiveCard target@(globals, playerIx) ref card =
  let
    playerLens :: Lens' Globals Player
    playerLens = lens get setter
      where
        get g = g^.players.to (!! playerIx)
        setter g p = g & players.ix playerIx .~ p
    refLens :: Lens' Globals Player
    refLens = lens get setter
      where
        get g = g^.players.to (!! ref)
        setter g p = g & players.ix ref .~ p
    player = globals^.playerLens
    fromPlayer = globals^.refLens
    handSize = fromPlayer^.playerHand.to length
    Just location = globals^.playerLocations.at player
    Just otherLocation = globals^.playerLocations.at fromPlayer
    playerHasCard = PlayerCard location `elem` player^.playerHand
  in
    if location == otherLocation && playerHasCard then
      Just $ target
      & _1.playerLens.playerHand %~ filter (/= PlayerCard location)
      & _1.refLens.playerHand %~ (PlayerCard location:)
      & if handSize + 1 > handLimit then
          _1.refLens.playerHand %~ filter (/= card)
        else
          id
    else
      Nothing

coTakeCard :: Target -> PlayerCard -> Maybe Target
coTakeCard target@(globals, playerIx) card =
  let
    playerLens :: Lens' Globals Player
    playerLens = lens get setter
      where
        get g = g^.players.to (!! playerIx)
        setter g p = g & players.ix playerIx .~ p
    Just location = globals^.playerLocations.at (globals^.playerLens)
  in
    case find (\p -> PlayerCard location `elem` p^.playerHand) (globals^.players) of
      Nothing -> Nothing
      Just other ->
        Just $ target
        & _1.playerLens.playerHand %~ (PlayerCard location:)
        & _1.players.traversed.filtered (== other).playerHand
        %~ filter (/= PlayerCard location)
        & if target^._1.playerLens.playerHand.to length > handLimit then
            _1.playerLens.playerHand %~ filter (/= card)
          else
            id

coDiscoverCure :: Target -> Lens' Player [City] -> Maybe Target
coDiscoverCure target@(globals, playerIx) ref =
  let
    cards = globals^.players.to (!! playerIx).ref
    playerLens :: Lens' Globals Player
    playerLens = lens get setter
      where
        get g = g^.players.to (!! playerIx)
        setter g p = g & players.ix playerIx .~ p
  in
    case Data.List.uncons cards of
      Nothing -> Nothing
      Just (x,xs) ->
        let
          color = colorOfCity x
        in
          if all (== color) $ map colorOfCity xs then
            Just $ target
            & _1.playerLens.playerHand %~ (\\ map PlayerCard cards)
            & if globals^.diseaseSupply.diseasesOfColor color
                 == diseasesAmount color then
                _1.cures.cureStatus color .~ Eradicated
              else
                id
          else
            Nothing

coRoleAbility :: Target -> Ability -> Maybe Target
coRoleAbility target@(globals, playerLens) ability = -- TODO
  Nothing
