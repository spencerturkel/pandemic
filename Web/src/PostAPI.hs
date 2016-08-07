{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module PostAPI where

import Control.Applicative
import Control.Concurrent.STM
import Control.Lens
import Control.Monad.Except
import Servant

import Globals

type PostAPI =
  "researchStationSupply" :> Capture "inc" Int :> PutNoContent '[JSON] ()

postAPI :: TVar Globals -> Server PostAPI
postAPI =
      setResearchStationSupply

setResearchStationSupply :: TVar Globals -> Int -> Handler ()
setResearchStationSupply g n =
  do
    modified <- liftIO . atomically $
      True <$ modifyTVar g (researchStationSupply .~ n) <|>
      return False
    unless modified $
      throwError err500 { errBody = "Couldn't modify globals due to concurrent access." }
