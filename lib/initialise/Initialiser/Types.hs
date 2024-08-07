{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Initialiser.Types
  ( Initialiser,
    runInitialiser,
  )
where

import Configuration (Configuration)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (LoggingT, MonadLogger, runStderrLoggingT)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)

newtype Initialiser a = Initialiser
  { run :: LoggingT (ReaderT Configuration IO) a
  }
  deriving (Applicative, Functor, Monad, MonadIO, MonadLogger, MonadReader Configuration, MonadThrow)

runInitialiser :: Initialiser a -> Configuration -> IO a
runInitialiser initialiser = runReaderT (runStderrLoggingT $ run initialiser)
