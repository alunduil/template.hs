module GitSpec (spec) where

import Data.Text (pack)
import qualified Git as SUT
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "Git" $ do
  describe "config" $ do
    it "gets remote.origin.url" $ do
      origin <- SUT.config "remote.origin.url"
      origin `shouldBe` pack "https://github.com/alunduil/template.hs.git"
    it "gets user.name" $ do
      name <- SUT.config "user.name"
      name `shouldBe` pack "Alex Brandt"
    it "get user.email" $ do
      email <- SUT.config "user.email"
      email `shouldBe` pack "alunduil@gmail.com"
