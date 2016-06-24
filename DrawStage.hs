{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RankNTypes #-}

module DrawStage where

import Control.Lens hiding (uncons)
import Control.Monad.Except
import Control.Monad.State
import Data.List

import City
import Deck
import Diseases
import Exception
import Globals
import Interpreter
import Player
import PlayerCard

drawStage ::
  (Interpreter m, MonadError Loseable m, MonadState Globals m)
  => Lens' Globals Player
  -> m ()
drawStage ref = do
  (epidemics, newCards) <-
    partition (== Epidemic)
    <$> replicateM 2 drawFromPlayerDeck
  unless (null epidemics) $ do
    doEpidemic
    when (length epidemics > 1) $ do
      --promptEvent TODO
      doEpidemic
  handSize <- fmap length $ ref.playerHand <<>= newCards
  when (handSize > 7) $ do
    card <- getCard
    ref.playerHand %= filter (/= card)

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
