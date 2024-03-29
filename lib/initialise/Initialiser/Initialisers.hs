module Initialiser.Initialisers
  ( defaultInitialiser,
  )
where

import qualified Cabal (replace)
import qualified File (replace)
import Initialiser.Types (Initialiser)
import qualified Licence (replace)
import System.FilePath ((</>))

defaultInitialiser :: Initialiser ()
defaultInitialiser = do
  mapM_
    File.replace
    [ ".devcontainer" </> "devcontainer.json",
      ".github" </> "workflows" </> "haskell-ci.yml",
      "CHANGELOG.md"
    ]
  Licence.replace "LICENSE"
  Cabal.replace "templatise.cabal"

-- TODO README
