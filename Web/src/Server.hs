{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Control.Applicative
import Control.Concurrent.STM
import Control.Lens
import Control.Monad.Except
import Network.Wai.Handler.Warp (run, Port)
import Servant

import GetAPI
import Globals
import GlobalsConfig
import GlobalsView (GlobalsView, viewGlobals)
import PostAPI

type PandemicAPI =
  GetAPI :<|>
  PostAPI

pandemic :: TVar Globals -> Server PandemicAPI
pandemic g =
    getAPI g :<|>
    postAPI g

runServer :: Port -> GlobalsConfig -> IO ()
runServer port conf =
  do
    g <- newTVarIO $ makeGlobals conf
    run port . serve (Proxy :: Proxy PandemicAPI) $ pandemic g
