{-# LANGUAGE OverloadedStrings #-}

module DefaultsSpec (spec) where

import Data.Maybe (fromJust)
import qualified Defaults as SUT
import Hooks (withGitRepo)
import Network.URI (parseURI)
import Test.Hspec (Spec, around, describe, it, shouldBe)

spec :: Spec
spec = describe "Defaults" $ do
  describe "dName" $ do
    it "converts origin to project name" $
      SUT.dName defaults `shouldBe` "repository"
  describe "dHomePage" $ do
    it "converts origin to a home page" $
      SUT.dHomePage defaults `shouldBe` fromJust (parseURI "http://github.com/username/repository")
  describe "getDefaults" $ around withGitRepo $ do
    it "inspects the current repository" $ \p -> do
      ds <- SUT.getDefaults
      ds
        `shouldBe` SUT.Defaults
          { SUT.dOrigin = fromJust (parseURI "https://github.com/sentinel/sentinel.git"),
            SUT.dAuthor = "Sentinel",
            SUT.dMaintainer = "sentinel@example.com",
            SUT.dPath = p,
            SUT.dYear = 2023
          }

defaults :: SUT.Defaults
defaults =
  SUT.Defaults
    { SUT.dOrigin = fromJust (parseURI "http://github.com/username/repository.git"),
      SUT.dAuthor = "Forename Surname",
      SUT.dMaintainer = "username@example.com",
      SUT.dPath = ".",
      SUT.dYear = 1970
    }
