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
  showTarget _ = return ()
  getAction (g, _) = RandomIO $ do
    i <- randomRIO (0, length (g^.spaces) - 1)
    return . Drive $ g ^?! spaces.ix i.city
  getCard target = RandomIO $ do
    i <- randomRIO (0, length (target^.playerLens.playerHand) - 1)
    let card =  target ^?! playerLens.playerHand.ix i
    print $ "getCard returns " <> show card
    return card
  notifyAll = RandomIO . mapM_ print
