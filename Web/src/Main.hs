module Main where

import Server
import Test

main :: IO ()
main =
  do
    conf <- testConfig
    runServer 8080 conf
