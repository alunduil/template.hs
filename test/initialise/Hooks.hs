module Hooks (withGitRepo) where

import Control.Monad (void)
import System.Directory (withCurrentDirectory)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcess)

withGitRepo :: (FilePath -> IO ()) -> IO ()
withGitRepo action =
  withSystemTempDirectory "initialise" $ \p ->
    withCurrentDirectory p $ do
      void $ readProcess "git" ["init"] ""
      void $ readProcess "git" ["config", "user.name", "Sentinel"] ""
      void $ readProcess "git" ["config", "user.email", "sentinel@example.com"] ""
      void $ readProcess "git" ["remote", "add", "origin", "https://github.com/sentinel/sentinel.git"] ""
      action p
