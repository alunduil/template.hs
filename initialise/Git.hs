module Git (config) where

import Control.Monad.Extra (pureIf)
import System.Exit (ExitCode (ExitSuccess))
import System.Process (readProcessWithExitCode)

config :: String -> IO (Maybe String)
config key = do
  (exitCode, stdout, _stderr) <- readProcessWithExitCode "git" ["config", key] ""
  pure $ pureIf (exitCode == ExitSuccess) $ head $ lines stdout
