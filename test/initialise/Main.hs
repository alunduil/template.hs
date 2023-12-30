module Main (main) where

import qualified CabalGolden (golden)
import qualified ConfigurationSpec (spec)
import qualified DefaultsSpec (spec)
import qualified FileGolden (golden)
import qualified GitSpec (spec)
import qualified InitialiseSpec (spec)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Hspec (testSpecs)

main :: IO ()
main = do
  specs <-
    concat
      <$> mapM
        testSpecs
        [ ConfigurationSpec.spec,
          DefaultsSpec.spec,
          GitSpec.spec,
          InitialiseSpec.spec
        ]
  goldens <- sequence [CabalGolden.golden, FileGolden.golden]
  defaultMain $
    testGroup
      "initialise-library"
      [ testGroup "Specs" specs,
        testGroup "Golden" goldens
      ]
