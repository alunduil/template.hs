module Initialise
  ( Initialise,
    runInitialise,
    relativePath,
    replaceIf,
    replace,
  )
where

import Control.Monad.Reader (ReaderT, runReaderT)
import GHC.IO.Encoding (TextDecoder)

type Initialise = ReaderT MetaData IO

runInitialise :: Initialise () -> IO ()
runInitialise = runReaderT

contents :: FilePath -> Initialise Text
contents path = undefined
