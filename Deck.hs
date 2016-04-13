module Deck where

import Control.Monad.State
import Data.Function
import Data.List
import qualified Data.Map as Map
import System.Random


newtype Deck a
  = Deck { _getDeck :: [a]
         }
  deriving (Show, Read)

addToDeck :: a -> Deck a -> Deck a
addToDeck a (Deck as) = Deck $ a:as

drawFromDeck :: Deck a -> (Maybe a, Deck a)
drawFromDeck (Deck []) = (Nothing, Deck [])
drawFromDeck (Deck (a:as)) = (Just a, Deck as)

stackSmallToBig :: [Deck a] -> Deck a
stackSmallToBig
  = Deck
  . concat
  . sortBy (flip compare `on` length)
  . map _getDeck

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