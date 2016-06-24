{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module GlobalsConfig where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Reader
import qualified Data.Map.Lazy       as Map
import           System.Random

import           City
import           Cures
import           Deck
import           Diseases
import Globals
import           Player
import           PlayerCard

data EpidemicNumber
  = Four
  | Five
  | Six
  deriving (Show, Read)

fromEpidemicNumber :: (MonadReader EpidemicNumber m) => m Int
fromEpidemicNumber = do
  n <- ask
  return $ case n of
    Four -> 4
    Five -> 5
    Six -> 6

data GlobalsConfig
  = GlobalsConfig { _initGen :: StdGen
                  , _initPlayers :: [Player]
                  , _numEpidemics :: EpidemicNumber
                  }
    deriving (Show, Read)
makeLenses ''GlobalsConfig

makeGlobals :: (MonadReader GlobalsConfig m) => m Globals
makeGlobals = do
  gen <- asks $ view initGen
  p <- asks $ view initPlayers
  epiNum <- asks $ view numEpidemics
  let
    initial = Globals { _spaces = Map.fromList $
                        map (flip (,) (Diseases 0 0 0 0)) [minBound..maxBound]
                      , _researchLocations = Map.fromList $
                        map ((\(c,b) -> if c == Atlanta then (c, True) else (c,b))
                        . flip (,) False) [minBound..maxBound]
                      , _players = p
                      , _playerLocations = Map.fromList $
                        map (flip (,) Atlanta) p
                      , _infectionRateCounter = minBound
                      , _outbreakCounter = minBound
                      , _cures = Cures Uncured Uncured Uncured Uncured
                      , _diseaseSupply = Diseases 24 24 24 24
                      , _researchStationSupply = 5
                      , _infectionDeck = Deck [minBound..maxBound]
                      , _infectionDiscard = Deck []
                      , _playerDeck = Deck $ map PlayerCard [minBound..maxBound]
                      , _playerDiscard = Deck []
                      , _generator = gen
                      , _eventEffects = []
                      }
  return $ initial &~ do
    modify $ shuffleDeck infectionDeck
    modify $ shuffleDeck playerDeck
    doInitialInfections
    Right hands <-
      runExceptT . replicateM (length p) . flip replicateM (drawFrom playerDeck)
      $ case compare (length p) 3 of LT -> 4
                                     EQ -> 3
                                     GT -> 2
    players %= zipWith (set playerHand) hands
    runReaderT insertEpidemics epiNum

    -- do initial infections, draw 3 and put 3 on each, draw 3 and put 2 on each, draw 3 and put 1 on each
doInitialInfections :: (MonadState Globals m) => m ()
doInitialInfections = runReaderT go 3
  where
    go :: (MonadReader Int m, MonadState Globals m) => m ()
    go = do
      n <- ask
      unless (n < 1) $ do
        Right city <- runExceptT (drawFrom infectionDeck)
        replicateM_ n
          . fmap (either undefined id)
          . runExceptT
          . infect city
          $ colorOfCity city
        local (\x -> x - 1) go

insertEpidemics :: (MonadReader EpidemicNumber m, MonadState Globals m) => m ()
insertEpidemics = do
  n <- fromEpidemicNumber <$> ask
  deck <- use playerDeck
  gen <- use generator
  let stacks = splitInto n deck & map (shuffle . addToDeck Epidemic)
      (shuffledStacks, gen') = runState (sequence stacks) gen
  generator .= gen'
  playerDeck .= stackSmallToBig shuffledStacks
