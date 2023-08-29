{-# LANGUAGE OverloadedStrings #-}

module ConfigurationSpec (spec) where

import qualified Configuration as SUT
import Data.Maybe (fromJust)
import Defaults (Defaults (..))
import Network.URI (parseURI)
import Options.Applicative (ParserFailure, ParserHelp, ParserResult (CompletionInvoked, Failure, Success), defaultPrefs, execParserPure, failureCode, helper, info, renderFailure, (<**>))
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import Test.Hspec (Expectation, Spec, describe, expectationFailure, it, shouldBe, shouldNotBe)

spec :: Spec
spec =
  describe "Configuration" $ do
    describe "parser" $ do
      it "should error if homepage isn't a URI" $
        parse ["--homepage", "not-a-url"] `shouldFailWith` ("", ExitFailure 1)
      it "should error if licence isn't an SPDX licence ID" $ do
        parse ["--licence", "not-a-licence"] `shouldFailWith` ("", ExitFailure 1)

parse :: [String] -> ParserResult SUT.Configuration
parse =
  execParserPure
    defaultPrefs
    ( info (SUT.parser defaults <**> helper)
    )

shouldFailWith :: ParserResult SUT.Configuration -> (String, ExitCode) -> Expectation
shouldFailWith (Success _) _ = expectationFailure "Expected Failure but got Success"
shouldFailWith (CompletionInvoked _) _ = expectationFailure "Expected Failure but got CompletionInvoked"
shouldFailWith (Failure f) rhs = renderFailure f "" `shouldBe` rhs

defaults :: Defaults
defaults =
  Defaults
    { dOrigin = fromJust (parseURI "http://github.com/username/repository.git"),
      dAuthor = "Forename Surname",
      dMaintainer = "username@example.com",
      dPath = ".",
      dYear = 1970
    }
