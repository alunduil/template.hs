module CabalGolden (golden) where

import Data.Maybe (fromJust)
import qualified Defaults as SUT
import Network.URI (parseURI)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Tasty (TestTree)
import Test.Tasty.Golden (TestTree)

golden :: IO TestTree
golden =
  testGroup "Cabal" $
    testGroup "convert" . map convertTest <$> findByExtension [".cabal"] d
  where
    d = normalise "test/initialise/data"

convertTest :: FilePath -> TestTree
convertTest path = goldenVsStringDiff name diff golden action
  where
    name = takeBaseName path
    diff golden output = ["diff", "-u", golden, output]
    golden = path `replaceExtension` ".golden.cabal"
    action = SUT.convert =<< SUT.contents path
