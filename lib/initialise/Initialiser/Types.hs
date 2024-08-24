{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Initialiser.Types
  ( Initialiser,
    runInitialiser,
  )
where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (LoggingT, MonadLogger, runStderrLoggingT)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import qualified Environment (T)

newtype Initialiser a = Initialiser
  { run :: LoggingT (ReaderT Environment.T IO) a
  }
  deriving (Applicative, Functor, Monad, MonadIO, MonadLogger, MonadReader Environment.T, MonadThrow)

runInitialiser :: Initialiser a -> Environment.T -> IO a
runInitialiser initialiser = runReaderT (runStderrLoggingT $ run initialiser)
