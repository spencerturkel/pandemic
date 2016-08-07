{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module GetAPI where

import Control.Concurrent.STM
import Control.Monad.Except
import Servant

import Globals (Globals)
import GlobalsView

type GetAPI =
  Get '[JSON] GlobalsView :<|>
  "researchStationSupply" :> Get '[JSON] Int

getAPI :: TVar Globals -> Server GetAPI
getAPI g =
  showGlobals g :<|>
  showSupply g

showGlobals :: TVar Globals -> Handler GlobalsView
showGlobals = liftIO . fmap viewGlobals . readTVarIO

showSupply :: TVar Globals -> Handler Int
showSupply = fmap _researchStationSupply . showGlobals
