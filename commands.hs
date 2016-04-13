module Commands where

import Control.Monad.Free

data CommandF a
  = ActionF a
