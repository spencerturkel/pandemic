{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module PostAPI where

import Control.Applicative
import Control.Concurrent.STM
import Control.Lens
import Control.Monad.Except
import Servant

import Globals

type PostAPI =
  "researchStationSupply" :> Capture "inc" Int :> Post '[JSON] Bool

postAPI :: TVar Globals -> Server PostAPI
postAPI =
      setResearchStationSupply

setResearchStationSupply :: TVar Globals -> Int -> Handler Bool
setResearchStationSupply g n =
    liftIO . atomically $
      True <$ modifyTVar g (researchStationSupply .~ n) <|>
      return False
