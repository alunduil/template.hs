{-# LANGUAGE RecordWildCards #-}

module Licence (replace) where

import Configuration (Configuration (..))
import Control.Monad (unless)
import Control.Monad.Reader (ask, liftIO)
import Data.ByteString.Lazy (ByteString, writeFile)
import Distribution.SPDX.LicenseId (LicenseId (Unlicense), licenseId)
import Initialise.Types (Initialise)
import Network.HTTP.Client (responseBody)
import Network.HTTP.Simple (httpLBS, parseRequest)
import System.FilePath ((</>))
import Prelude hiding (writeFile)

replace :: FilePath -> Initialise ()
replace p = do
  Configuration {..} <- ask
  unless (licence == Unlicense) $
    liftIO (writeFile (path </> p) =<< contents licence)

contents :: LicenseId -> IO ByteString
contents l = do
  request <- parseRequest $ "https://spdx.org/licenses/" ++ licenseId l ++ ".txt"
  responseBody <$> httpLBS request
