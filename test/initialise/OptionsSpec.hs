{-# LANGUAGE OverloadedStrings #-}

module OptionsSpec (spec) where

import Data.Text (Text, isInfixOf, pack)
import Defaults (Defaults (..))
import qualified Environment (T)
import qualified Options as SUT
import Options.Applicative
  ( ParserResult
      ( CompletionInvoked,
        Failure,
        Success
      ),
    defaultPrefs,
    execParserPure,
    renderFailure,
  )
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import Test.Hspec (Spec, describe, it, shouldSatisfy)

spec :: Spec
spec =
  describe "Options" $ do
    describe "parserInfo" $ do
      it "should error if homepage isn't a URI" $
        parse ["--homepage", "not-a-url"]
          `shouldSatisfy` failWith "option --homepage: cannot parse value `not-a-url'"
      it "should error if licence isn't an SPDX licence ID" $ do
        parse ["--licence", "not-a-licence"]
          `shouldSatisfy` failWith "option --licence: cannot parse value `not-a-licence'"

failWith :: Text -> ParserResult a -> Bool
failWith _ (Success _) = False
failWith _ (CompletionInvoked _) = False
failWith m (Failure f) = case renderFailure f "" of
  (_, ExitSuccess) -> False
  (m', ExitFailure _) -> m `isInfixOf` pack m'

parse :: [String] -> ParserResult Environment.T
parse =
  execParserPure defaultPrefs (SUT.parserInfo defaults)

defaults :: Defaults
defaults =
  Defaults
    { dOrigin = "http://github.com/username/repository.git",
      dAuthor = "Forename Surname",
      dMaintainer = "username@example.com",
      dPath = ".",
      dYear = 1970
    }
