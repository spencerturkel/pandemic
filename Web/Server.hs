{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Control.Monad.Except
import Network.Wai.Handler.Warp (run)
import Servant

import GlobalsView
import Test

type PandemicAPI
  = "test" :> Get '[JSON] GlobalsView

pandemic :: Server PandemicAPI
pandemic
  = test

test :: Handler GlobalsView
test = liftIO $ viewGlobals <$> testGlobals

runServer :: Server PandemicAPI -> IO ()
runServer = run 8080 . serve (Proxy :: Proxy PandemicAPI)
