{-# LANGUAGE OverloadedStrings #-}

module InitialiseSpec (spec) where

import Control.Monad.Logger (LogLevel (LevelDebug))
import Data.Maybe (fromJust)
import Data.Text (Text, pack)
import Distribution.SPDX (LicenseId (MIT))
import qualified Environment (T (..))
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
            Environment.T
              { Environment.name = "sentinel",
                Environment.cabalName = "sentinel",
                Environment.homepage = fromJust (parseURI "https://github.com/sentinel/sentinel.git"),
                Environment.author = "Sentinel",
                Environment.maintainer = "sentinel@example.com",
                Environment.licence = MIT,
                Environment.path = p,
                Environment.year = 1970,
                Environment.verbosity = LevelDebug
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
