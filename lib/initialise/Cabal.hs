{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cabal (replace, convert) where

import Configuration (Configuration (..))
import Control.Exception (Exception)
import Control.Monad.Catch (throwM)
import Control.Monad.Reader (asks, liftIO)
import Data.ByteString (ByteString, append, breakSubstring, concat, readFile, stripPrefix)
import qualified Data.ByteString.Char8 as BS (pack)
import Data.Text (Text, unlines)
import qualified Data.Text as T (pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.IO (writeFile)
import Distribution.Fields
  ( CommentPosition (NoComment),
    Field (Field, Section),
    FieldLine (FieldLine),
    Name (Name),
    SectionArg (SecArgName, SecArgStr),
    fromParsecFields,
    readFields,
    showFields,
  )
import Distribution.Fields.Field (fieldLineAnn)
import Distribution.Parsec.Position (Position)
import Distribution.SPDX (licenseId)
import Initialise (Initialise)
import System.Directory.Extra (createDirectoryIfMissing, removeDirectoryRecursive, removeFile)
import System.FilePath (replaceBaseName, (</>))
import Text.Parsec.Error (ParseError)
import Prelude hiding (concat, readFile, unlines, writeFile)

instance Exception ParseError

replace :: FilePath -> Initialise ()
replace path = do
  replaceCabal path
  "lib" `replaceDirectoryWith` replaceLib
  "test" `replaceDirectoryWith` replaceTest
  "bin" `replaceDirectoryWith` replaceBin

replaceCabal :: FilePath -> Initialise ()
replaceCabal path = do
  -- TODO handle in replaceWith
  path' <- asks (flip replaceBaseName path . T.unpack . name)
  -- TODO replaceWith convert
  contents <- liftIO $ readFile path
  contents' <- convert contents
  liftIO $ writeFile path' contents'
  -- TODO handle in replaceWith
  liftIO $ removeFile path

convert :: ByteString -> Initialise Text
convert contents = do
  fs <- either throwM pure (readFields contents)
  T.pack . showFields (const NoComment) . fromParsecFields <$> mapM convert' fs

convert' :: Field Position -> Initialise (Field Position)
convert' f@(Field n@(Name _ fName) ls) = do
  Configuration {..} <- asks id
  case fName of
    -- package
    "name" -> field (encodeUtf8 name)
    "version" -> field "0.1.0.0"
    "license" -> field (BS.pack $ licenseId licence)
    "copyright" -> field (BS.pack $ unwords ["(c)", show year, T.unpack author])
    "author" -> field (encodeUtf8 author)
    "maintainer" -> field (encodeUtf8 maintainer)
    "homepage" -> field $ BS.pack $ show homepage
    "bug-reports" -> field $ BS.pack (show homepage ++ "/issues")
    "synopsis" -> field "TODO"
    "description" -> field "TODO"
    -- common
    "import" -> field (encodeUtf8 name `append` "-common")
    "exposed-modules" -> field ""
    "other-modules" -> field ""
    "build-depends" -> pure $ Field n $ map (convertFieldLine name) ls
    "hs-source-dirs" -> pure $ Field n $ map (convertFieldLine name) ls
    -- source-repository
    "location" -> field $ BS.pack $ show homepage
    _ -> pure f
  where
    field s = pure $ Field n [FieldLine annotation s]
    annotation = fieldLineAnn . head $ ls
convert' (Section n arguments fs) = do
  Configuration {..} <- asks id
  fs' <- mapM convert' fs
  pure $ Section n (map (convertSectionArgument name) arguments) fs'

convertSectionArgument :: Text -> SectionArg Position -> SectionArg Position
convertSectionArgument n s = case s of
  (SecArgName a o) -> SecArgName a $ convertString n o
  (SecArgStr a o) -> SecArgStr a $ convertString n o
  _ -> s

convertFieldLine :: Text -> FieldLine Position -> FieldLine Position
convertFieldLine r (FieldLine annotation s) = FieldLine annotation $ convertString r s

convertString :: Text -> ByteString -> ByteString
convertString r s = case token `stripPrefix` rest of
  Just suffix -> concat [prefix, encodeUtf8 r, suffix]
  Nothing -> s
  where
    (prefix, rest) = token `breakSubstring` s
    token = "initialise"

-- TODO Move to Initialise module?
replaceDirectoryWith :: FilePath -> (FilePath -> Initialise ()) -> Initialise ()
replaceDirectoryWith component r = do
  name' <- asks name
  path' <- asks ((</> component </> T.unpack name') . path)
  liftIO $ createDirectoryIfMissing True path'
  r path'
  liftIO $ removeDirectoryRecursive $ replaceBaseName "initialise" path'

replaceLib :: FilePath -> Initialise ()
replaceLib _path = pure ()

replaceTest :: FilePath -> Initialise ()
replaceTest path = do
  name' <- asks name
  -- TODO Template library.
  liftIO $
    writeFile (path </> "Main.hs") $
      unlines
        [ "module Main (main) where",
          "",
          "import Test.Tasty (defautMain, testGroup)",
          "",
          "main :: IO ()",
          "main = defaultMain $ testGroup \"" <> name' <> "-library\" []"
        ]

replaceBin :: FilePath -> Initialise ()
replaceBin path = do
  name' <- asks name
  -- TODO Template library.(*)
  liftIO $
    writeFile (path </> "Main.hs") $
      unlines
        [ "module Main (main) where",
          "",
          "main :: IO ()",
          "main = putStrLn " <> name'
        ]
