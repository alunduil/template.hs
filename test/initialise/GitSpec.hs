{-# LANGUAGE OverloadedStrings #-}

module GitSpec (spec) where

import Data.Text (pack)
import qualified Git as SUT
import Hooks (withGitRepo)
import Test.Hspec (Spec, around, describe, it, shouldBe)

spec :: Spec
spec = around withGitRepo $
  describe "Git" $ do
    describe "config" $ do
      it "gets remote.origin.url" $ const $ do
        origin <- SUT.config "remote.origin.url"
        origin `shouldBe` pack "https://github.com/sentinel/sentinel.git"
      it "gets user.name" $ const $ do
        name <- SUT.config "user.name"
        name `shouldBe` pack "Sentinel"
      it "get user.email" $ const $ do
        email <- SUT.config "user.email"
        email `shouldBe` pack "sentinel@example.com"
