{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Deck where

import Control.Lens
import Control.Monad.Except
import Control.Monad.State
import Data.Function
import Data.List
import qualified Data.Map as Map
import System.Random

data DeckException
  = DrawFromEmptyDeck
  deriving (Show, Read)

newtype Deck a
  = Deck { _getDeck :: [a]
         }
  deriving (Show, Read)
makeLenses ''Deck

addToDeck :: a -> Deck a -> Deck a
addToDeck a (Deck as) = Deck $ a:as

drawFromDeck :: (MonadError DeckException m, MonadState (Deck a) m) => m a
drawFromDeck = do
  (Deck d) <- get
  case d of
    [] -> throwError DrawFromEmptyDeck
    (a:as) -> do
      put (Deck as)
      return a

stackSmallToBig :: [Deck a] -> Deck a
stackSmallToBig
  = Deck
  . concat
  . sortBy (flip compare `on` length)
  . map (view getDeck)

splitInto :: Int -> Deck a -> [Deck a]
splitInto n (Deck xs)
  | n < 1 = return $ Deck xs
  | otherwise =
      let
        stackSize = length xs `div` n
        go :: Int -> [a] -> [[a]]
        go size cards =
          case splitAt size cards of
            (taken, []) -> [taken]
            (taken, rest) -> taken : go size rest
      in map Deck $ go stackSize xs

shuffle :: (MonadState g m, RandomGen g) => Deck a -> m (Deck a)
shuffle (Deck xs) = do
  g <- get
  let (x, g') = fisherYates g xs
  put g'
  return $ Deck x
  where
    -- pure shuffle found on Wiki
    fisherYates :: RandomGen g => g -> [a] -> ([a], g)
    fisherYates gen [] = ([], gen)
    fisherYates gen l =
      toElems $ Prelude.foldl fisherYatesStep (initial (head l) gen) (numerate (tail l))
    toElems (x, y) = (Map.elems x, y)
    numerate = zip [1..]
    initial x generator = (Map.singleton 0 x, generator)
    fisherYatesStep :: RandomGen g => (Map.Map Int a, g) -> (Int, a) -> (Map.Map Int a, g)
    fisherYatesStep (m, theGen) (i, x) = ((Map.insert j x . Map.insert i (m Map.! j)) m, theGen')
      where
          (j, theGen') = randomR (0, i) theGen
