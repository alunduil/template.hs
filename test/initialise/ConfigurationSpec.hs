{-# LANGUAGE OverloadedStrings #-}

module ConfigurationSpec (spec) where

import qualified Configuration as SUT
import Data.String.Utils (strip)
import Defaults (Defaults (..))
import Options.Applicative
  ( ParserResult
      ( CompletionInvoked,
        Failure,
        Success
      ),
    defaultPrefs,
    execParserPure,
    helper,
    info,
    renderFailure,
    (<**>),
  )
import System.Exit (ExitCode (ExitFailure))
import Test.Hspec (Expectation, Spec, describe, expectationFailure, it, shouldBe)

spec :: Spec
spec =
  describe "Configuration" $ do
    describe "parser" $ do
      it "should error if homepage isn't a URI" $
        parse ["--homepage", "not-a-url"]
          `shouldFailWith` ( strip $
                               unlines
                                 [ "option --homepage: cannot parse value `not-a-url'",
                                   "",
                                   "Usage:  [--name NAME] [--homepage URL] [--author AUTHOR] ",
                                   "        [--maintainer MAINTAINER] [--licence LICENCE]"
                                 ],
                             ExitFailure 1
                           )
      it "should error if licence isn't an SPDX licence ID" $ do
        parse ["--licence", "not-a-licence"]
          `shouldFailWith` ( strip $
                               unlines
                                 [ "option --licence: cannot parse value `not-a-licence'",
                                   "",
                                   "Usage:  [--name NAME] [--homepage URL] [--author AUTHOR] ",
                                   "        [--maintainer MAINTAINER] [--licence LICENCE]"
                                 ],
                             ExitFailure 1
                           )

parse :: [String] -> ParserResult SUT.Configuration
parse =
  execParserPure
    defaultPrefs
    ( info (SUT.parser defaults <**> helper) mempty
    )

shouldFailWith :: ParserResult SUT.Configuration -> (String, ExitCode) -> Expectation
shouldFailWith (Success _) _ = expectationFailure "Expected Failure but got Success"
shouldFailWith (CompletionInvoked _) _ = expectationFailure "Expected Failure but got CompletionInvoked"
shouldFailWith (Failure f) rhs = renderFailure f "" `shouldBe` rhs

defaults :: Defaults
defaults =
  Defaults
    { dOrigin = "http://github.com/username/repository.git",
      dAuthor = "Forename Surname",
      dMaintainer = "username@example.com",
      dPath = ".",
      dYear = 1970
    }
