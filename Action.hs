{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}

module Action where

--import Control.Comonad.Cofree
--import Control.Lens
import Control.Monad.Trans.Free

import City
import Diseases
--import Globals
import Player
import PlayerCard

data Ability
  = ContingencyPlannerEvent PlayerCard
  | DispatcherMoveOtherPawnToCityWithAnotherPawn Player Player
  | DispatcherMoveAnotherPawnAsIfOwn Player City
  | OperationsExpertBuildResearch
  | OperationsExpertMoveFromResearch City
  deriving (Show, Read)

data ActionF k
  = Drive City (Bool -> k)
  | DirectFlight City (Bool -> k)
  | CharterFlight City (Bool -> k)
  | ShuttleFlight City (Bool -> k)
  | Build (Bool -> k)
  | Treat (Bool -> k)
  | GiveCard (Bool -> k)
  | TakeCard (Bool -> k)
  | DiscoverCure DiseaseColor (Bool -> k)
  | RoleAbility Ability (Bool -> k)
  deriving (Functor)

type ActionT m a = FreeT ActionF m a

drive :: Monad m => City -> ActionT m Bool
drive city = liftF $ Drive city id

directFlight :: Monad m => City -> ActionT m Bool
directFlight city = liftF $ DirectFlight city id

charterFlight :: Monad m => City -> ActionT m Bool
charterFlight city = liftF $ CharterFlight city id

shuttleFlight :: Monad m => City -> ActionT m Bool
shuttleFlight city = liftF $ ShuttleFlight city id

build :: Monad m => ActionT m Bool
build = liftF $ Build id

treat :: Monad m => ActionT m Bool
treat = liftF $ Treat id

giveCard :: Monad m => ActionT m Bool
giveCard = liftF $ GiveCard id

takeCard :: Monad m => ActionT m Bool
takeCard = liftF $ TakeCard id

discoverCure :: Monad m => DiseaseColor -> ActionT m Bool
discoverCure color = liftF $ DiscoverCure color id

roleAbility :: Monad m => Ability -> ActionT m Bool
roleAbility ability = liftF $ RoleAbility ability id

{- -- Using a CoFree interpreter, see Dave Laing's blog

data CoActionF k
  = CoActionF { _driveH :: City -> (Bool, k)
              , _directFlightH :: City -> (Bool, k)
              , _charterFlightH :: City -> (Bool, k)
              , _shuttleFlightH :: City -> (Bool, k)
              , _buildH :: (Bool, k)
              , _treatH :: (Bool, k)
              , _giveCardH :: (Bool, k)
              , _takeCardH :: (Bool, k)
              , _discoverCureH :: DiseaseColor -> (Bool, k)
              , _roleAbilityH :: Ability -> (Bool, k)
              }
  deriving (Functor)
makeLenses ''CoActionF

type CoAction a = Cofree CoActionF a

mkCoAction :: (Globals, Player) -> CoAction (Globals, Player)
mkCoAction = coiter go
  where
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

co :: (Globals, Player) -> City -> (Bool, Globals)
co :: (Globals, Player) -> City -> (Bool, Globals)
co :: (Globals, Player) -> City -> (Bool, Globals)
co :: (Globals, Player) -> City -> (Bool, Globals)
co :: (Globals, Player) -> (Bool, Globals)
co :: (Globals, Player) -> (Bool, Globals)
co :: (Globals, Player) -> (Bool, Globals)
co :: (Globals, Player) -> (Bool, Globals)
co :: (Globals, Player) -> DiseaseColor -> (Bool, Globals)
co :: (Globals, Player) -> Ability -> (Bool, Globals)

-}
