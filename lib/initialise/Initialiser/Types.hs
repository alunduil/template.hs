{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Initialiser.Types
  ( Initialiser,
    runInitialiser,
  )
where

import Configuration (Configuration)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)

newtype Initialiser a = Initialiser
  { run :: ReaderT Configuration IO a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadThrow,
      MonadReader Configuration
    )

runInitialiser :: Initialiser a -> Configuration -> IO a
runInitialiser initialiser = runReaderT (run initialiser)
