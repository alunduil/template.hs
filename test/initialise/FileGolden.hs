{-# LANGUAGE OverloadedStrings #-}

module FileGolden (golden) where

import Control.Monad.Logger (LogLevel (LevelDebug))
import Control.Monad.Reader (liftIO)
import Data.Maybe (fromJust)
import Data.Text.IO (readFile)
import Data.Text.Lazy (fromStrict)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Distribution.SPDX.LicenseId (LicenseId (MIT))
import qualified Environment (T (..))
import qualified File as SUT
import Initialiser (runInitialiser)
import Network.URI (parseURI)
import System.FilePath (isExtensionOf, normalise, replaceExtension, takeBaseName, takeExtension)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (findByExtension, goldenVsStringDiff)
import Prelude hiding (readFile)

golden :: IO TestTree
golden = do
  testGroup "File.convert"
    <$> mapM
      g
      [ "devcontainer.json",
        "CHANGELOG.md",
        "haskell-ci.yml"
      ]
  where
    d = normalise "test/initialise/data"
    g n =
      let ext = takeExtension n
       in testGroup n
            . map convertTest
            . filter (not . ((".golden" ++ ext) `isExtensionOf`))
            <$> findByExtension [ext] d

convertTest :: FilePath -> TestTree
convertTest p = goldenVsStringDiff n diff gold action
  where
    n = takeBaseName p
    diff a b = ["diff", "-u", a, b]
    gold = p `replaceExtension` ".golden" ++ takeExtension p
    action = do
      contents <- liftIO (readFile p)
      encodeUtf8 . fromStrict <$> runInitialiser (SUT.convert contents) configuration
    configuration =
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
