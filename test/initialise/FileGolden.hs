{-# LANGUAGE OverloadedStrings #-}

module FileGolden (golden) where

import Configuration (Configuration (..))
import Control.Monad.Reader (liftIO)
import Data.Maybe (fromJust)
import Data.Text.IO (readFile)
import Data.Text.Lazy (fromStrict)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Distribution.SPDX.LicenseId (LicenseId (MIT))
import qualified File as SUT
import Initialise (runInitialise)
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
      encodeUtf8 . fromStrict <$> runInitialise (SUT.convert contents) configuration
    configuration =
      Configuration
        { name = "sentinel",
          homepage = fromJust (parseURI "https://github.com/sentinel/sentinel.git"),
          author = "Sentinel",
          maintainer = "sentinel@example.com",
          licence = MIT,
          path = ".",
          year = 1970
        }
