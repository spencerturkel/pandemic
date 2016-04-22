{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Action where

import Control.Applicative
import Control.Comonad.Cofree
import Control.Lens
import Control.Monad
import Control.Monad.Trans.Free
import Data.List

import City
import Cures
import Diseases
import Globals
import Player
import PlayerCard

data Ability
  = ContingencyPlannerEvent PlayerCard
  | DispatcherMoveOtherPawnToCityWithAnotherPawn Player Player
  | DispatcherMoveAnotherPawnAsIfOwn Player City
  | OperationsExpertBuildResearch
  | OperationsExpertMoveFromResearch City
  deriving (Show, Read)

type PlayerRef = Lens' Globals Player

data ActionF k
  = Drive City (Bool -> k)
  | DirectFlight City (Bool -> k)
  | CharterFlight City (Bool -> k)
  | ShuttleFlight City (Bool -> k)
  | Build City (Bool -> k)
  | Treat DiseaseColor (Bool -> k)
  | GiveCard PlayerRef (Bool -> k)
  | TakeCard PlayerCard (Bool -> k)
  | DiscoverCure (Lens' Player [City]) (Bool -> k)
  | RoleAbility Ability (Bool -> k)
  deriving (Functor)

type ActionT m a = FreeT ActionF m a

--interpret :: Monad m => Globals -> Player -> ActionT m r -> m r
--interpret globals player action = do
--  act <- action
--  case act of
--    Pure x -> return x
--    Free (Drive city f) ->
--      let
--        canReach = citiesConnected city $ globals ^?! playerLocations.at player
--        newGlobals = if canReach then
--                       globals ^. playerLocations. at player .~ city
--                     else
--                       globals
--      in
--        interpret $ f canReach

drive :: Monad m => City -> ActionT m Bool
drive city = liftF $ Drive city id

directFlight :: Monad m => City -> ActionT m Bool
directFlight city = liftF $ DirectFlight city id

charterFlight :: Monad m => City -> ActionT m Bool
charterFlight city = liftF $ CharterFlight city id

shuttleFlight :: Monad m => City -> ActionT m Bool
shuttleFlight city = liftF $ ShuttleFlight city id

build :: Monad m => City -> ActionT m Bool
build city = liftF $ Build city id

treat :: Monad m => DiseaseColor -> ActionT m Bool
treat color = liftF $ Treat color id

giveCard :: Monad m => PlayerRef -> ActionT m Bool
giveCard ref = liftF $ GiveCard ref id

takeCard :: Monad m => PlayerCard -> ActionT m Bool
takeCard c = liftF $ TakeCard c id

discoverCure :: Monad m => Lens' Player [City] -> ActionT m Bool
discoverCure ref = liftF $ DiscoverCure ref id

roleAbility :: Monad m => Ability -> ActionT m Bool
roleAbility ability = liftF $ RoleAbility ability id

 -- Using a CoFree interpreter, see Dave Laing's blog

data CoActionF k
  = CoActionF { _driveH :: City -> (Bool, k)
              , _directFlightH :: City -> (Bool, k)
              , _charterFlightH :: City -> (Bool, k)
              , _shuttleFlightH :: City -> (Bool, k)
              , _buildH :: City -> (Bool, k)
              , _treatH :: DiseaseColor -> (Bool, k)
              , _giveCardH :: PlayerRef -> (Bool, k)
              , _takeCardH :: PlayerCard -> (Bool, k)
              , _discoverCureH :: Lens' Player [City] -> (Bool, k)
              , _roleAbilityH :: Ability -> (Bool, k)
              }
makeLenses ''CoActionF

instance Functor CoActionF where
  fmap f co = CoActionF { _driveH = fmap f <$> _driveH co
                        , _directFlightH = fmap f <$> _directFlightH co
                        , _charterFlightH = fmap f <$> _charterFlightH co
                        , _shuttleFlightH = fmap f <$> _shuttleFlightH co
                        , _buildH = fmap f <$> _buildH co
                        , _treatH = fmap f <$> _treatH co
                        , _giveCardH = fmap f <$> _giveCardH co
                        , _takeCardH = fmap f <$> _takeCardH co
                        , _discoverCureH = fmap f <$> _discoverCureH co
                        , _roleAbilityH = fmap f <$> _roleAbilityH co
                        }

type CoAction a = Cofree CoActionF a

type Target = (Globals, Lens' Globals Player)

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

coDrive :: Target -> City -> (Bool, Target)
coDrive target@(globals, playerLens) city =
  let
    player = globals^.playerLens
  in
    case citiesConnected city <$> globals^.playerLocations.at player of
      Just True -> (True, target & _1.playerLocations.at player ?~ city)
      _ -> (False, target)

coDirectFlight :: Target -> City -> (Bool, Target)
coDirectFlight target@(globals, playerLens) city =
  let
    player = globals^.playerLens
    card = PlayerCard city
  in
    if card `elem` player^.playerHand then
      (True, target
       & _1.playerLocations.at player ?~ city
       & _1.playerLens.playerHand %~ filter (/= card))
      else
      (False, target)

coCharterFlight :: Target -> City -> (Bool, Target)
coCharterFlight target@(globals, playerLens) city =
  let
    player = globals^.playerLens
    Just location = PlayerCard <$> globals ^. playerLocations.at player
  in
    if location `elem` player^.playerHand then
      (True, target
       & _1.playerLocations.at player ?~ city
       & _1.playerLens.playerHand %~ filter (/= location))
    else
      (False, target)

coShuttleFlight :: Target -> City -> (Bool, Target)
coShuttleFlight target@(globals, playerLens) city =
  let
    player = globals^.playerLens
    Just location = globals^.playerLocations.at player
    researchAtLocation = globals^.researchLocations.at location
    researchAtCity = globals^.researchLocations.at city
  in
    if Just True == liftA2 (&&) researchAtCity researchAtLocation then
      (True, target & _1.playerLocations.at player ?~ city)
    else
      (False, target)

coBuild :: Target -> City -> (Bool, Target)
coBuild target@(globals, playerLens) city =
  let
    player = globals^.playerLens
    Just location = globals^.playerLocations.at player
    supplyEmpty = globals^.researchStationSupply == 1
  in
    if PlayerCard location `elem` player^.playerHand then
      let
        result = (True, target
                  & _1.researchLocations.at location ?~ True
                  & _1.playerLens.playerHand %~ filter (/= PlayerCard location))
      in
        if supplyEmpty then
          result & _2._1.researchLocations.at city ?~ False
        else
          result
    else
      (False, target)

coTreat :: Target -> DiseaseColor -> (Bool, Target)
coTreat target@(globals, playerLens) color =
  let
    player = globals^.playerLens
    Just location = globals^.playerLocations.at player
    Just diseases = globals^.spaces.at location
    count = diseases^.diseasesOfColor color
  in
    if count > 0 then
      if Cured == globals^.cures.cureStatus color then
        (True, target
         & _1.spaces.at location ?%~ diseasesOfColor color .~ 0
         &~ replicateM_ count (_1.diseaseSupply %= addDisease color)
         & _1.cures.cureStatus color %~
         if globals^.diseaseSupply.diseasesOfColor color
            == diseasesAmount color then
           const Eradicated
         else
           id
        )
      else
        (True, target
         & _1.spaces.at location ?%~ removeDisease color
         & _1.diseaseSupply %~ addDisease color)
    else
      (False, target)

coGiveCard :: Target -> PlayerRef -> (Bool, Target)
coGiveCard target@(globals, playerLens) ref =
  let
    player = globals^.playerLens
    fromPlayer = globals^.ref
    Just location = globals^.playerLocations.at player
    Just otherLocation = globals^.playerLocations.at fromPlayer
    playerHasCard = PlayerCard location `elem` player^.playerHand
  in
    if location == otherLocation && playerHasCard then
      (True, target
       & _1.playerLens.playerHand %~ filter (/= PlayerCard location)
       & _1.ref.playerHand %~ (PlayerCard location:))
    else
      (False, target)

coTakeCard :: Target -> PlayerCard -> (Bool, Target)
coTakeCard target@(globals, playerLens) card =
  let
    player = globals^.playerLens
    Just location = globals^.playerLocations.at player
  in
    case find (\p -> PlayerCard location `elem` p^.playerHand) (globals^.players) of
      Nothing -> (False, target)
      Just other ->
        (True, target
         & _1.playerLens.playerHand %~ (PlayerCard location:)
         & _1.players.traversed.filtered (== other).playerHand
         %~ filter (/= PlayerCard location)
         & if player^.playerHand.to length > handLimit then
             _1.playerLens.playerHand %~ filter (/= card)
           else
             id
        )

coDiscoverCure :: Target -> Lens' Player [City] -> (Bool, Target)
coDiscoverCure target@(globals, playerLens) ref =
  let
    cards = globals^.playerLens.ref
  in
    case Data.List.uncons cards of
      Nothing -> (False, target)
      Just (x,xs) ->
        let
          color = colorOfCity x
        in
          if all (== color) $ map colorOfCity xs then
            (True, target
             & _1.playerLens.playerHand %~ (\\ map PlayerCard cards)
             & if globals^.diseaseSupply.diseasesOfColor color
                  == diseasesAmount color then
                 _1.cures.cureStatus color .~ Eradicated
               else
                 id)
          else
            (False, target)

coRoleAbility :: Target -> Ability -> (Bool, Target)
coRoleAbility target@(globals, playerLens) ability = -- TODO
  undefined
