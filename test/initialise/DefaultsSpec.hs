{-# LANGUAGE OverloadedStrings #-}

module DefaultsSpec (spec) where

import Data.Maybe (fromJust)
import qualified Defaults as SUT
import Network.URI (parseURI)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "Defaults" $ do
  describe "dName" $ do
    it "converts origin to project name" $
      SUT.dName defaults `shouldBe` "repository"
  describe "dHomePage" $ do
    it "converts origin to a home page" $
      SUT.dHomePage defaults `shouldBe` fromJust (parseURI "http://github.com/username/repository")
  describe "getDefaults" $ do
    it "inspects the current repository" $ do
      ds <- SUT.getDefaults
      ds
        `shouldBe` SUT.Defaults
          { SUT.dOrigin = fromJust (parseURI "https://github.com/alunduil/template.hs.git"),
            SUT.dAuthor = "Alex Brandt",
            SUT.dMaintainer = "alunduil@gmail.com",
            SUT.dPath = "/workspaces/template.hs",
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
