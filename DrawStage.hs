{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RankNTypes #-}

module DrawStage where

import Control.Lens hiding (uncons)
import Control.Monad.Except
import Control.Monad.State
import Data.List

import City
import Cures
import Deck
import Diseases
import Exception
import Globals
import Player
import PlayerCard

drawStage ::
  (MonadError Loseable m, MonadState Globals m)
  => Lens' Globals Player
  -> m ()
drawStage ref = do
  (epidemics, handCards) <-
    partition (== Epidemic)
    <$> replicateM 2 drawFromPlayerDeck
  replicateM_ (length epidemics) $ do
    doEpidemic
    --promptEvent TODO
  hand <- ref.playerHand <<>= handCards
  return ()
  -- when (length hand > 7) $ promptDiscard hand TODO

drawFromPlayerDeck :: (MonadError Loseable m, MonadState Globals m) => m PlayerCard
drawFromPlayerDeck = do
  card <-
    runExceptT (drawFrom playerDeck
                :: (MonadState Globals m, MonadError DeckException m)
                   => m PlayerCard)
  either (const $ throwError PlayerDeckExhausted) return card

doEpidemic :: (MonadError Loseable m, MonadState Globals m) => m ()
doEpidemic = do
  infectionRateCounter %= succ
  Just (city, deck) <- uncons . reverse <$> use (infectionDeck.getDeck)
  epidemicInfect city
  infectionDiscard %= addToDeck city
  modify $ shuffleDeck infectionDiscard
  discarded <- use (infectionDiscard.getDeck)
  infectionDiscard .= Deck []
  infectionDeck .= Deck (discarded ++ reverse deck)

epidemicInfect :: (MonadError Loseable m, MonadState Globals m) => City -> m ()
epidemicInfect city = do
  let color = colorOfCity city
      spaceLens = spaces.at city.non undefined.diseasesOfColor color
  count <- use spaceLens
  if count == 0 then
    replicateM_ 3 $ infect city color
    else
    replicateM_ (4 - count) $ infect city color
