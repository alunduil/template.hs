{-# LANGUAGE OverloadedStrings #-}

module GitSpec (spec) where

import Data.Maybe (fromJust)
import Data.Text (pack)
import qualified Git as SUT
import Network.URI (parseURI)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "Git" $ do
  describe "config" $ do
    it "gets remote.origin.url" $
      SUT.config "remote.origin.url" `shouldBe` (pack . fromJust (parseURI "https://github.com/alunduil/template.hs.git"))
    it "gets user.name" $
      SUT.config "user.name" `shouldBe` "Alex Brandt"
    it "get user.email" $
      SUT.config "user.email" `shouldBe` "alunduil@gmail.com"
