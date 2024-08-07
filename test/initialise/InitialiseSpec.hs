{-# LANGUAGE OverloadedStrings #-}

module InitialiseSpec (spec) where

import Configuration (Configuration (..))
import Control.Monad.Logger (LogLevel (LevelDebug))
import Data.Maybe (fromJust)
import Data.Text (Text, pack)
import Distribution.SPDX (LicenseId (MIT))
import Hooks (withProjectCopy)
import qualified Initialiser as SUT
import Network.URI (parseURI)
import System.Directory (doesDirectoryExist, doesFileExist)
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
                cabalName = "sentinel",
                homepage = fromJust (parseURI "https://github.com/sentinel/sentinel.git"),
                author = "Sentinel",
                maintainer = "sentinel@example.com",
                licence = MIT,
                path = p,
                year = 1970,
                verbosity = LevelDebug
              }

      SUT.runInitialiser SUT.defaultInitialiser configuration

      doesFileExist (p </> "templatise.cabal") `shouldReturn` False
      doesFileExist (p </> "sentinel.cabal") `shouldReturn` True

      doesDirectoryExist (p </> "bin" </> "initialise") `shouldReturn` False
      doesFileExist (p </> "bin" </> "sentinel" </> "Main.hs") `shouldReturn` True

      doesDirectoryExist (p </> "lib" </> "initialise") `shouldReturn` False
      doesDirectoryExist (p </> "lib" </> "sentinel") `shouldReturn` True

      doesDirectoryExist (p </> "test" </> "initialise") `shouldReturn` False
      doesFileExist (p </> "test" </> "sentinel" </> "Main.hs") `shouldReturn` True

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
              [ "CHANGELOG.md",
                ".devcontainer" </> "devcontainer.json",
                ".github" </> "workflows" </> "haskell-ci.yml",
                "sentinel.cabal",
                ".vscode" </> "launch.json",
                ".vscode" </> "tasks.json"
              ]

      grep ps fs `shouldReturn` []

grep :: [String] -> [FilePath] -> IO [Text]
grep ps fs = map pack . lines <$> readProcess "grep" ("-o" : unlines ps : fs) ""
