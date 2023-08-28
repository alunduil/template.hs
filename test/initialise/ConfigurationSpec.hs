module MetaDataSpec (spec) where

import qualified MetaData as SUT
import Options.Applicative (execParserPure)
import Test.Hspec (Spec, describe, it)

spec :: Spec
spec =
  describe "MetaData" $ do
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

defaults :: SUT.Defaults
defaults =
  SUT.Defaults
    { SUT.dOrigin = fromJust (parseURI "http://github.com/username/repository.git"),
      SUT.dAuthor = "Forename Surname",
      SUT.dMaintainer = "username@example.com",
      SUT.dPath = ".",
      SUT.dYear = 1970
    }
