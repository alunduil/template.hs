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
    [ "CHANGELOG.md",
      ".devcontainer" </> "devcontainer.json",
      ".github" </> "workflows" </> "haskell-ci.yml",
      ".vscode" </> "launch.json",
      ".vscode" </> "tasks.json"
    ]
  Licence.replace "LICENSE"
  Cabal.replace "templatise.cabal"

-- TODO README
