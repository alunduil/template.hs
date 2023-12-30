{-# LANGUAGE OverloadedStrings #-}

module InitialiseSpec (spec) where

import Configuration (Configuration (..))
import Data.Maybe (fromJust)
import Data.Text (Text, pack)
import Distribution.SPDX (LicenseId (MIT))
import Hooks (withProjectCopy)
import qualified Initialise as SUT
import Network.URI (parseURI)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.Process (readProcess)
import Test.Hspec (Spec, describe, runIO, shouldNotReturn, shouldReturn)

spec :: Spec
spec = describe "Initialisers" $ do
  describe "defaultInitialiser" $ do
    runIO $ withProjectCopy $ \p -> do
      let configuration =
            Configuration
              { name = "sentinel",
                homepage = fromJust (parseURI "https://github.com/sentinel/sentinel.git"),
                author = "Sentinel",
                maintainer = "sentinel@example.com",
                licence = MIT,
                path = p,
                year = 1970
              }

      SUT.runInitialise SUT.defaultInitialiser configuration

      doesFileExist (p </> "templatise.cabal") `shouldReturn` False
      doesFileExist (p </> "sentinel.cabal") `shouldReturn` True

      grep ["MIT"] [p </> "LICENSE"] `shouldNotReturn` []

      let ps =
            [ "initialise",
              "template-hs",
              "template.hs",
              "templatise"
            ]

      let fs =
            map
              (p </>)
              [ ".devcontainer" </> "devcontainer.json",
                ".github" </> "workflows" </> "haskell-ci.yml",
                "CHANGELOG.md",
                "sentinel.cabal"
              ]

      grep ps fs `shouldReturn` []

grep :: [String] -> [FilePath] -> IO [Text]
grep ps fs = map pack . lines <$> readProcess "grep" ("-o" : unlines ps : fs) ""
