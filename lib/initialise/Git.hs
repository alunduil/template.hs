module Git
  ( config,
    root,
  )
where

import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)
import Data.Text (Text, pack, unpack)
import System.Process (readProcess)

config :: Text -> IO Text
config key = do
  output <- nonEmpty . lines <$> readProcess "git" ["config", unpack key] []
  case output of
    Just (x :| _) -> pure $ pack x
    Nothing -> fail $ "`git config " <> unpack key <> "` returned no output"

root :: IO Text
root = do
  output <- nonEmpty . lines <$> readProcess "git" ["rev-parse", "--show-toplevel"] ""
  case output of
    Just (x :| _) -> pure $ pack x
    Nothing -> fail "`git rev-parse --show-toplevel` returned no output"
