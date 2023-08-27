module Main (main) where

import qualified DefaultsSpec (spec)
import qualified GitSpec (spec)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Hspec (testSpecs)

main :: IO ()
main = do
  specs <-
    concat
      <$> mapM
        testSpecs
        [ DefaultsSpec.spec,
          GitSpec.spec
        ]
  defaultMain $
    testGroup
      "initialise-library"
      [ testGroup "Specs" specs
      ]
