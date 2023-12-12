module Git (config) where

import Data.Text (Text, pack)
import System.Process (readProcess)

config :: Text -> IO Text
config key = pack . head . lines <$> readProcess "git" ["config", key] ""
