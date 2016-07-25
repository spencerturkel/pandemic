module Test where

import City
import Globals
import GlobalsConfig
import Player

import System.Random

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(f .: g) x y = f (g x y)
infixr 9 .:

globalsConfigIO :: [Player] -> EpidemicNumber -> IO GlobalsConfig
globalsConfigIO xs epiNum = do
  gen <- newStdGen
  return $ GlobalsConfig gen xs epiNum

globalsIO :: [Player] -> EpidemicNumber -> IO Globals
globalsIO = fmap makeGlobals .: globalsConfigIO

testConfig :: IO GlobalsConfig
testConfig = globalsConfigIO [Player 0 [] Scientist Atlanta] Four

testGlobals :: IO Globals
testGlobals = makeGlobals <$> testConfig
