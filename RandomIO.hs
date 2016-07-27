{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module RandomIO where

import Control.Lens
import Data.Foldable
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
    i <- randomRIO (0, length (g^.spaces) - 1)
    let drive = Drive $ g ^?! spaces.ix i.city
    print $ "getAction for player " <> show p <> " returns " <> show drive
    return drive
  getCard target = RandomIO $ do
    i <- randomRIO (0, length (target^.playerLens.playerHand) - 1)
    let card =  target ^?! playerLens.playerHand.ix i
    print $ "getCard returns " <> show card
    return card
  notify xs = RandomIO . for_ xs $ \x ->
    putStrLn $ "Got notification: " <> show x
