{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}
module Changelogged.Versions.Bump where

import Turtle
import Prelude hiding (FilePath)

import Control.Exception hiding (catch)
import qualified Control.Foldl as Fold
import Control.Monad.Catch

import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)

import Filesystem.Path.CurrentOS (encodeString)
import System.Console.ANSI (Color(..))

import Changelogged.Versions.Utils
import Changelogged.Types
import Changelogged.Options
import Changelogged.Utils
import Changelogged.Pure
import Changelogged.Pattern
import Changelogged.Config

-- |Get current local version.
currentLocalVersion :: VersionFile -> Appl Text
currentLocalVersion VersionFile{..} = do
  ver <- fold (grep (has pattern) (input versionFilePath)) Fold.head
  return $ case ver of
    Just realVer -> fromMaybe
      (throw (PatternMatchFail $ "cannot get local version. Given variable " <>
                                 show (versionPatternVariable versionFileVersionPattern) <>
                                 " doesn't store version. Check config.\n"))
      (versionMatch . lineToText $ realVer)
    Nothing -> throw (PatternMatchFail $ "cannot get local version. Cannot match given pattern " <>
                                         show versionFileVersionPattern <>
                                         " in file " <> encodeString versionFilePath <>
                                         ". Check config.\n")
  where
    pattern = text (versionPatternVariable versionFileVersionPattern) <> spaces <> text (versionPatternSeparator versionFileVersionPattern)

-- |Generate new local version.
generateLocalVersionForFile :: Level -> VersionFile -> Appl Text
generateLocalVersionForFile lev indicator = do
  current <- currentLocalVersion indicator
  -- This print must not be here but I think it's better than throw current vrsion to main.
  printf ("Version: "%s%" -> ") current
  coloredPrint Yellow (new current <> "\n")
  return (new current)
  where
    new current = bump (delimited current) lev

-- |Set new local version.
generateLocalVersion :: Level -> ChangelogConfig -> Appl (Maybe Text)
generateLocalVersion lev ChangelogConfig{..} = do
  case changelogVersionFiles of
    Nothing -> error "No file version files specified for changelog."
    Just versionFiles -> do
      localVersions <- mapM (generateLocalVersionForFile lev) versionFiles
      return (listToMaybe localVersions) -- FIXME: don't ignore other version files

-- |Infer new local version.
generateLocalVersionByChangelog :: ChangelogConfig -> Appl (Maybe Text)
generateLocalVersionByChangelog logConfig@ChangelogConfig{..} = do
  versionedChanges <- getLevelOfChanges changelogChangelog changelogLevelHeaders
  case versionedChanges of
    Just lev -> generateLocalVersion lev logConfig
    Nothing -> do
      warning $ "keeping current version since " <> showPath changelogChangelog <> " does not contain any new level headers or even entries."
      return Nothing

bumpVersions :: Bool -> ChangelogConfig -> Appl ()
bumpVersions upToDate config@ChangelogConfig{..} = do
  Options{..} <- ask
  when optBumpVersions $ if
    | not upToDate && not optForce ->
        failure $ "cannot bump versions because " <> format fp changelogChangelog <> " is out of date.\nUse --no-check to skip changelog checks.\nUse --force to force bump version."
    | not upToDate && optForce ->
        warning $ format fp changelogChangelog <> " is out of date. Bumping versions anyway due to --force."
    | otherwise -> (do
        newVersion <- case (optNoCheck, optChangeLevel) of
          (_, Just lev) -> generateLocalVersion lev config
          (True, Nothing) ->  do
            failure "cannot infer new version from changelog because of --no-check.\nUse explicit --level CHANGE_LEVEL."
            return Nothing
          (False, Nothing) -> generateLocalVersionByChangelog config

        case newVersion of
          Nothing -> return ()
          Just version -> case changelogVersionFiles of
            Just versionFiles -> do
              mapM_ (bumpPart version) versionFiles
              headChangelog version changelogChangelog
            Nothing -> warning "no files specified to bump versions in"
        ) `catch` (\(ex :: PatternMatchFail) -> failure (showText ex))
