module Environment (T (..)) where

import Control.Monad.Logger (LogLevel)
import Data.Text (Text)
import Data.Time.Calendar (Year)
import Distribution.SPDX.LicenseId (LicenseId)
import Network.URI (URI)

data T = T
  { name :: Text,
    cabalName :: Text,
    homepage :: URI,
    author :: Text,
    maintainer :: Text,
    licence :: LicenseId,
    path :: FilePath,
    year :: Year,
    verbosity :: LogLevel
  }
  deriving (Show)
