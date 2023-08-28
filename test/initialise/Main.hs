module Main (main) where

import qualified CabalGolden (golden)
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
  goldens <- sequence [CabalGolden.golden]
  defaultMain $
    testGroup
      "initialise-library"
      [ testGroup "Specs" specs,
        testGroup "Golden" goldens
      ]
