module Licence (convert) where

import Configuration (MetaData (..))
import Data.ByteString.Lazy (ByteString, writeFile)
import Distribution.SPDX.LicenseId (LicenseId (Unlicense), licenseId)
import Network.HTTP.Client (responseBody)
import Network.HTTP.Simple (httpLBS, parseRequest)
import Prelude hiding (writeFile)

convert :: MetaData -> FilePath -> IO ()
convert MetaData {licence = Unlicense} _path = pure ()
-- TODO Fill in LICENCE template values (i.e., <year>)
convert MetaData {licence = licence} path = writeFile path =<< text licence

text :: LicenseId -> IO ByteString
text licence = do
  request <- parseRequest $ "https://spdx.org/licenses/" ++ licenseId licence ++ ".txt"
  responseBody <$> httpLBS request