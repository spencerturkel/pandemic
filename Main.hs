module Main where

import GameLoop
import RandomIO
import Test

main :: IO ()
main = do
  g <- testGlobals
  g' <- runRandomIO $ run g
  print g'
