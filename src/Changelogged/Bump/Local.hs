module Changelogged.Bump.Local where

import Turtle
import Prelude hiding (FilePath)

import Control.Exception
import qualified Control.Foldl as Fold

import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)

import Filesystem.Path.CurrentOS (encodeString)
import System.Console.ANSI (Color(..))

import Changelogged.Types
import Changelogged.Utils
import Changelogged.Pure
import Changelogged.Pattern
import Changelogged.Bump.Common
import Changelogged.Config

-- |Get current local version.
currentLocalVersion :: VersionFile -> IO Text
currentLocalVersion VersionFile{..} = do
  ver <- fold (grep (has (text versionFileVersionPattern)) (input versionFilePath)) Fold.head
  return $ case ver of
    Just realVer -> fromMaybe
      (throw (PatternMatchFail $ "cannot get local version. Given variable " <> show versionFileVersionPattern <> " doesn't store version. Check config.\n"))
      (versionMatch . lineToText $ realVer)
    Nothing -> throw (PatternMatchFail $ "cannot get local version. Cannot find given variable " <> show versionFileVersionPattern <> " in file " <> encodeString versionFilePath <> ". Check config.\n")

-- |Generate new local version.
generateLocalVersionForFile :: Level -> VersionFile -> IO Text
generateLocalVersionForFile lev indicator = do
  current <- currentLocalVersion indicator
  -- This print must not be here but I think it's better than throw current vrsion to main.
  printf ("Version: "%s%" -> ") current
  coloredPrint Yellow (new current <> "\n")
  return (new current)
  where
    new current = bump (delimited current) lev

-- |Set new local version.
generateLocalVersion :: Level -> ChangelogConfig -> IO (Maybe Text)
generateLocalVersion lev ChangelogConfig{..} = do
  case changelogVersionFiles of
    Nothing -> error "No file version files specified for changelog."
    Just versionFiles -> do
      localVersions <- mapM (generateLocalVersionForFile lev) versionFiles
      return (listToMaybe localVersions) -- FIXME: don't ignore other version files

-- |Infer new local version.
generateLocalVersionByChangelog :: ChangelogConfig -> IO (Maybe Text)
generateLocalVersionByChangelog logConfig@ChangelogConfig{..} = do
  versionedChanges <- getChangelogEntries changelogChangelog
  case versionedChanges of
    Just lev -> generateLocalVersion lev logConfig
    Nothing -> do
      warning $ "keeping current version since " <> showPath changelogChangelog <> " apparently does not contain any new entries"
      return Nothing
