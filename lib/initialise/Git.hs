module Git (config) where

import Data.Text (Text, pack)
import System.Process (readProcess)

config :: String -> IO Text
config key = pack . head . lines <$> readProcess "git" ["config", key] ""
