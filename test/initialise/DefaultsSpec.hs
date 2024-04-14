{-# LANGUAGE OverloadedStrings #-}

module DefaultsSpec (spec) where

import Data.Time (LocalTime (localDay), getCurrentTime, getCurrentTimeZone, utcToLocalTime)
import Data.Time.Calendar.OrdinalDate (toOrdinalDate)
import qualified Defaults as SUT
import Hooks (withGitRepo)
import Network.URI (parseURI)
import Test.Hspec (Spec, around, describe, it, shouldBe)

spec :: Spec
spec = describe "Defaults" $ do
  describe "dName" $ do
    it "converts httpOrigin to project name" $
      SUT.dName httpOrigin `shouldBe` Just "repository"
    it "converts sshOrigin to project name" $
      SUT.dName sshOrigin `shouldBe` Just "repository"
    it "converts twoExtensionsOrigin to project name" $
      SUT.dName twoExtensionsOrigin `shouldBe` Just "repository.two"
  describe "dHomePage" $ do
    it "converts httpOrigin to a home page" $
      SUT.dHomePage httpOrigin `shouldBe` parseURI "http://github.com/username/repository"
    it "converts sshOrigin to a home page" $
      SUT.dHomePage sshOrigin `shouldBe` parseURI "http://github.com/username/repository"
    it "converts twoExtensionsOrigin to a home page" $
      SUT.dHomePage twoExtensionsOrigin `shouldBe` parseURI "http://github.com/username/repository.two"
  describe "getDefaults" $ around withGitRepo $ do
    it "inspects the current repository" $ \p -> do
      ds <- SUT.getDefaults
      -- TODO resolve code duplication with SUT.
      timezone <- getCurrentTimeZone
      (dYear, _day) <- toOrdinalDate . localDay . utcToLocalTime timezone <$> getCurrentTime
      ds
        `shouldBe` SUT.Defaults
          { SUT.dOrigin = "https://github.com/sentinel/sentinel.git",
            SUT.dAuthor = "Sentinel",
            SUT.dMaintainer = "sentinel@example.com",
            SUT.dPath = p,
            SUT.dYear = dYear
          }

httpOrigin :: SUT.Defaults
httpOrigin =
  SUT.Defaults
    { SUT.dOrigin = "http://github.com/username/repository.git",
      SUT.dAuthor = "Forename Surname",
      SUT.dMaintainer = "username@example.com",
      SUT.dPath = ".",
      SUT.dYear = 1970
    }

sshOrigin :: SUT.Defaults
sshOrigin =
  SUT.Defaults
    { SUT.dOrigin = "git@github.com:username/repository.git",
      SUT.dAuthor = "Forename Surname",
      SUT.dMaintainer = "username@example.com",
      SUT.dPath = ".",
      SUT.dYear = 1970
    }

twoExtensionsOrigin :: SUT.Defaults
twoExtensionsOrigin =
  SUT.Defaults
    { SUT.dOrigin = "http://github.com/username/repository.two.git",
      SUT.dAuthor = "Forename Surname",
      SUT.dMaintainer = "username@example.com",
      SUT.dPath = ".",
      SUT.dYear = 1970
    }
