module Hooks (withGitRepo, withProjectCopy) where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Text (unpack)
import Git (root)
import System.Directory (getCurrentDirectory, withCurrentDirectory)
import System.FilePath (takeFileName, (</>))
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

withProjectCopy :: (FilePath -> IO ()) -> IO ()
withProjectCopy action = do
  withSystemTempDirectory "initialise" $ \p -> do
    r <- unpack <$> root <|> getCurrentDirectory
    void $ readProcess "cp" ["-a", r, p] ""
    let p' = p </> takeFileName r
    withCurrentDirectory p' $ action p'
