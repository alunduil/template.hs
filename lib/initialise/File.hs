{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module File (replace, convert) where

import Configuration (Configuration (..))
import Control.Monad.Reader (MonadIO (liftIO), MonadReader (ask))
import Data.Text (Text)
import qualified Data.Text as T (replace)
import Data.Text.IO (readFile, writeFile)
import Initialiser.Types (Initialiser)
import Prelude hiding (readFile, writeFile)

replace :: FilePath -> Initialiser ()
replace path = do
  -- TODO replaceWith convert
  contents <- liftIO $ readFile path
  contents' <- convert contents
  liftIO $ writeFile path contents'

convert :: Text -> Initialiser Text
convert contents = do
  Configuration {..} <- ask
  pure
    . T.replace "templatise" name
    . T.replace "template-hs" name
    . T.replace "template.hs" name
    $ contents
