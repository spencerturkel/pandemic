{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Control.Applicative
import Control.Concurrent.STM
import Control.Lens
import Control.Monad.Except
import Network.Wai.Handler.Warp (run, Port)
import Servant

import Globals
import GlobalsConfig
import GlobalsView (GlobalsView, viewGlobals)

type PandemicAPI =
  "test" :> Get '[JSON] GlobalsView :<|>
  "testMod" :> Capture "inc" Int :> Post '[JSON] Bool

pandemic :: TVar Globals -> Server PandemicAPI
pandemic g =
    test g :<|>
      testMod g

test :: TVar Globals -> Handler GlobalsView
test = liftIO . fmap viewGlobals . readTVarIO

testMod :: TVar Globals -> Int -> Handler Bool
testMod g n =
    liftIO . atomically $
      True <$ modifyTVar g (researchStationSupply .~ n) <|>
      return False

runServer :: Port -> GlobalsConfig -> IO ()
runServer port conf =
  do
    g <- newTVarIO $ makeGlobals conf
    run port . serve (Proxy :: Proxy PandemicAPI) $ pandemic g
