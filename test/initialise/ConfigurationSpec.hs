{-# LANGUAGE OverloadedStrings #-}

module ConfigurationSpec (spec) where

import qualified Configuration as SUT
import Data.Maybe (fromJust)
import Defaults (Defaults (..))
import Network.URI (parseURI)
import Options.Applicative (defaultPrefs, execParserPure, helper, info, renderFailure, (<**>))
import Test.Hspec (Spec, describe, it, shouldBe, shouldNotBe)

spec :: Spec
spec =
  describe "Configuration" $ do
    describe "parser" $ do
      it "should error if homepage isn't a URI" $ do
        (error, exitCode) <-
          renderFailure
            <$> execParserPure
              defaultPrefs
              (info (SUT.parser defaults <**> helper))
              ["--homepage", "not-a-url"]
        exitCode `shouldNotBe` 0
        error `shouldBe` ""
      it "should error if licence isn't an SPDX licence ID" $ do
        (error, exitCode) <-
          renderFailure
            <$> execParserPure
              defaultPrefs
              (info (SUT.parser defaults <**> helper))
              ["--licence", "not-a-licence"]
        exitCode `shouldNotBe` 0
        error `shouldBe` ""

defaults :: Defaults
defaults =
  Defaults
    { dOrigin = fromJust (parseURI "http://github.com/username/repository.git"),
      dAuthor = "Forename Surname",
      dMaintainer = "username@example.com",
      dPath = ".",
      dYear = 1970
    }
