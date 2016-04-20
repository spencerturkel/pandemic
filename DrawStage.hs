{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RankNTypes #-}

module DrawStage where

import Control.Lens
import Control.Monad.Except
import Control.Monad.State
import Data.List

import Deck(DeckException(..))
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
doEpidemic = undefined
