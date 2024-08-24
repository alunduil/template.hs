{-# LANGUAGE OverloadedStrings #-}

module CabalGolden (golden) where

import qualified Cabal as SUT
import Control.Monad.Logger (LogLevel (LevelDebug))
import Control.Monad.Reader (liftIO)
import Data.ByteString (readFile)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Maybe (fromJust)
import Data.Text (unpack)
import Distribution.SPDX.LicenseId (LicenseId (MIT))
import qualified Environment (T (..))
import Initialiser (runInitialiser)
import Network.URI (parseURI)
import System.FilePath (isExtensionOf, normalise, replaceExtension, takeBaseName)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (findByExtension, goldenVsStringDiff)
import Prelude hiding (readFile)

golden :: IO TestTree
golden =
  testGroup "Cabal.convert"
    . map convertTest
    . filter (not . (".golden.cabal" `isExtensionOf`))
    <$> findByExtension [".cabal"] d
  where
    d = normalise "test/initialise/data"

convertTest :: FilePath -> TestTree
convertTest p = goldenVsStringDiff n diff gold action
  where
    n = takeBaseName p
    diff a b = ["diff", "-u", a, b]
    gold = p `replaceExtension` ".golden.cabal"
    action = do
      contents <- liftIO (readFile p)
      pack . unpack <$> runInitialiser (SUT.convert contents) environment
    environment =
      Environment.T
        { Environment.name = "sentinel",
          Environment.cabalName = "sentinel",
          Environment.homepage = fromJust (parseURI "https://github.com/sentinel/sentinel.git"),
          Environment.author = "Sentinel",
          Environment.maintainer = "sentinel@example.com",
          Environment.licence = MIT,
          Environment.path = ".",
          Environment.year = 1970,
          Environment.verbosity = LevelDebug
        }
