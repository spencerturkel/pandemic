{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module RandomIO where

import Control.Lens
import Data.Monoid
import System.Random

import Action
import Globals
import Interpreter
import Player
import Space
import Target

newtype RandomIO a = RandomIO { runRandomIO :: IO a }
                   deriving (Functor, Applicative, Monad)

instance Interpreter RandomIO where
  showTarget _ = RandomIO $ print "showTarget called"
  getAction (g, p) = RandomIO $ do
    i <- randomRIO (0, length $ g^.spaces)
    let drive = Drive $ g ^?! spaces.ix i.city
    print $ "getAction for player " <> show p <> " returns " <> show drive
    return drive
  getCard target = RandomIO $ do
    i <- randomRIO (0, length $ target^.playerLens.playerHand)
    let card =  target ^?! playerLens.playerHand.ix i
    print $ "getCard for target@" <> show target <> " returns " <> show card
    return card
