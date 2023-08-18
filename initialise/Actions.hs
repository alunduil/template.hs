module Actions (convertCabal, convertFile) where

import Configuration (MetaData (..))
import Data.ByteString.Lazy (writeFile)
import Distribution.SPDX.LicenseId (LicenseId (Unlicense))
import Prelude hiding (writeFile)

convertCabal :: MetaData -> FilePath -> IO ()
convertCabal metadata cabal = undefined

convertFile :: MetaData -> FilePath -> IO ()
convertFile metadata file = undefined
