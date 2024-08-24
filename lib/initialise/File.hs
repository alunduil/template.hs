{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module File
  ( replace,
    convert,
  )
where

import Control.Monad.Logger (logInfo)
import Control.Monad.Reader (MonadIO (liftIO), MonadReader (ask))
import Data.Text (Text, pack)
import qualified Data.Text as T (replace)
import Data.Text.IO (readFile, writeFile)
import qualified Environment (T (..))
import Initialiser.Types (Initialiser)
import Prelude hiding (readFile, writeFile)

replace :: FilePath -> Initialiser ()
replace p = do
  $logInfo ("replacing file " <> pack (show p))
  -- TODO replaceWith convert
  contents <- liftIO $ readFile p
  contents' <- convert contents
  liftIO $ writeFile p contents'

convert :: Text -> Initialiser Text
convert contents = do
  Environment.T {..} <- ask
  pure
    . T.replace "templatise" name
    . T.replace "template-hs" name
    . T.replace "template.hs" name
    . T.replace "initialise" name
    $ contents
