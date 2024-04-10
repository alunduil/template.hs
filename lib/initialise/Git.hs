module Git
  ( config,
    root,
  )
where

import Data.Text (Text, pack, unpack)
import System.Process (readProcess)

config :: Text -> IO Text
config key = pack . head . lines <$> readProcess "git" ["config", unpack key] ""

root :: IO Text
root = pack . head . lines <$> readProcess "git" ["rev-parse", "--show-toplevel"] ""
