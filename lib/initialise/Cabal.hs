{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cabal (replace, convert) where

import Configuration (Configuration (author, licence, name, year))
import Control.Exception (Exception)
import Control.Monad.Catch (throwM)
import Control.Monad.Reader (asks, liftIO)
import Data.ByteString (ByteString, readFile)
import Data.ByteString.Char8 (pack)
import Data.Text (unpack)
import Data.Text.Encoding (encodeUtf8)
import Distribution.Fields (CommentPosition (NoComment), Field (Field, Section), FieldLine (FieldLine), Name (Name), fromParsecFields, readFields, showFields)
import Distribution.Fields.Field (fieldLineAnn)
import Distribution.Parsec.Position (Position)
import Distribution.SPDX (licenseId)
import Initialise (Initialise)
import System.Directory.Extra (removeFile)
import System.FilePath (replaceBaseName)
import Text.Parsec.Error (ParseError)
import Prelude hiding (readFile)

instance Exception ParseError

replace :: FilePath -> Initialise ()
replace path = do
  -- TODO handle in replaceWith
  path' <- asks (flip replaceBaseName path . unpack . name)
  -- TODO replaceWith convert
  contents <- liftIO $ readFile path
  contents' <- convert contents
  liftIO $ writeFile path' contents'
  -- TODO handle in replaceWith
  liftIO $ removeFile path

convert :: ByteString -> Initialise String
convert contents = do
  fs <- either throwM pure (readFields contents)
  showFields (const NoComment) . fromParsecFields <$> mapM convert' fs

convert' :: Field Position -> Initialise (Field Position)
convert' f@(Field n@(Name _ fName) ls) = do
  name' <- asks name
  licence' <- asks licence
  author' <- asks author
  year' <- asks year
  let annotation = fieldLineAnn . head $ ls
  case fName of
    "name" -> pure $ Field n [FieldLine annotation (encodeUtf8 name')]
    "version" -> pure $ Field n [FieldLine annotation "0.1.0.0"]
    "license" -> pure $ Field n [FieldLine annotation (pack $ licenseId licence')]
    "copyright" -> pure $ Field n [FieldLine annotation (unwords ["(c)", author', show year'])]
    _ -> pure f
convert' f@(Section n@(Name _ fName) arguments ls) = undefined

{-
instance ModifyWith PD.PackageDescription where
  modifyWith metadata pd =
    pd
      { PD.package = modifyWith metadata pd.package,
        PD.licenseRaw = Left . License . simpleLicenseExpression $ metadata.licence,
        PD.copyright = toShortText $ unwords ["(c)", show metadata.year, metadata.author],
        PD.maintainer = toShortText metadata.maintainer,
        PD.author = toShortText metadata.author,
        PD.homepage = toShortText $ show metadata.homepage,
        PD.bugReports = toShortText $ show metadata.homepage ++ "/issues",
        PD.sourceRepos =
          [ (emptySourceRepo RepoHead)
              { repoType = Just $ KnownRepoType Git,
                repoLocation = Just $ show metadata.homepage
              }
          ],
        PD.executables = modifyWith metadata <$> pd.executables,
        PD.testSuites = modifyWith metadata <$> pd.testSuites
      }

instance ModifyWith PackageIdentifier where
  modifyWith metadata _pi =
    PackageIdentifier
      { pkgName = mkPackageName $ name metadata,
        pkgVersion = mkVersion [0, 1, 0, 0]
      }

instance ModifyWith Executable where
  modifyWith metadata executable =
    executable
      { exeName = mkUnqualComponentName metadata.name,
        buildInfo = modifyWith metadata executable.buildInfo
      }

instance ModifyWith TestSuite where
  modifyWith metadata testSuite =
    testSuite
      { testName = mkUnqualComponentName $ metadata.name ++ "-test",
        testBuildInfo = modifyWith metadata testSuite.testBuildInfo
      }

instance ModifyWith BuildInfo where
  modifyWith metadata buildInfo =
    buildInfo
      { otherModules = [],
        targetBuildDepends = filter is_base buildInfo.targetBuildDepends
      }
    where
      is_base = ("base" ==) . unPackageName . depPkgName

-}
