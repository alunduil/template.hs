{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Licence
  ( replace,
  )
where

import Control.Monad (unless)
import Control.Monad.Logger (logInfo)
import Control.Monad.Reader (ask, liftIO)
import Data.ByteString.Lazy (ByteString, writeFile)
import Data.Text (pack)
import Distribution.SPDX.LicenseId (LicenseId (Unlicense), licenseId)
import qualified Environment (T (..))
import Initialiser.Types (Initialiser)
import Network.HTTP.Client (responseBody)
import Network.HTTP.Simple (httpLBS, parseRequest)
import System.FilePath ((</>))
import Prelude hiding (writeFile)

replace :: FilePath -> Initialiser ()
replace p = do
  Environment.T {..} <- ask
  unless (licence == Unlicense) $ do
    $logInfo ("replacing LICENSE " <> pack (show p))
    liftIO (writeFile (path </> p) =<< contents licence)

contents :: LicenseId -> IO ByteString
contents l = do
  request <- parseRequest $ "https://spdx.org/licenses/" ++ licenseId l ++ ".txt"
  responseBody <$> httpLBS request
