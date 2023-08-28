module Initialise
  ( Initialise,
    runInitialise,
  )
where

import Configuration (Configuration)
import Control.Monad.Reader (ReaderT, runReaderT)

type Initialise = ReaderT Configuration IO

runInitialise :: Initialise () -> Configuration -> IO ()
runInitialise = runReaderT
