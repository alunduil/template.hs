module Initialise.Types
  ( Initialise,
    runInitialise,
  )
where

import Configuration (Configuration)
import Control.Monad.Reader (ReaderT, runReaderT)

type Initialise = ReaderT Configuration IO

runInitialise :: Initialise a -> Configuration -> IO a
runInitialise = runReaderT
