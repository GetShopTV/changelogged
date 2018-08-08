{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}
module Changelogged.Versions.Bump where

import Turtle
import Prelude hiding (FilePath)

import Control.Exception hiding (catch)
import qualified Control.Foldl as Fold
import Control.Monad.Catch

import Data.Maybe (fromMaybe, listToMaybe)

import Filesystem.Path.CurrentOS (encodeString)
import System.Console.ANSI (Color(..))

import Changelogged.Versions.Utils
import Changelogged.Types
import Changelogged.Options
import Changelogged.Utils
import Changelogged.Pure
import Changelogged.Pattern
import Changelogged.Config

-- |Get current version.
currentVersion :: VersionFile -> Appl Version
currentVersion VersionFile{..} = do
  ver <- fold (grep (has pattern) (input versionFilePath)) Fold.head
  return $ case ver of
    Just realVer -> fromMaybe
      (throw (PatternMatchFail $ "cannot get local version. Given variable " <>
                                 show (versionPatternVariable versionFileVersionPattern) <>
                                 " doesn't store version. Check config.\n"))
      $ fmap Version . versionMatch . lineToText $ realVer
    Nothing -> throw (PatternMatchFail $ "cannot get local version. Cannot match given pattern " <>
                                         show versionFileVersionPattern <>
                                         " in file " <> encodeString versionFilePath <>
                                         ". Check config.\n")
  where
    pattern = text (versionPatternVariable versionFileVersionPattern) <> spaces <> text (versionPatternSeparator versionFileVersionPattern)

-- |Generate new local version.
generateVersionForFile :: Level -> VersionFile -> Appl Version
generateVersionForFile lev indicator = do
  (Version current) <- currentVersion indicator
  -- This print must not be here but I think it's better than throw current vrsion to main.
  printf ("Version: "%s%" -> ") current
  coloredPrint Yellow (getVersion (new current) <> "\n")
  return (new current)
  where
    new current = Version $ bump (delimited current) lev

-- |Set new local version.
generateVersion :: Level -> ChangelogConfig -> Appl (Maybe Version)
generateVersion lev ChangelogConfig{..} = do
  case changelogVersionFiles of
    Nothing -> error "No file version files specified for changelog."
    Just versionFiles -> do
      versions <- mapM (generateVersionForFile lev) versionFiles
      return (listToMaybe versions)

-- |Infer new local version.
generateVersionByChangelog :: ChangelogConfig -> Appl (Maybe Version)
generateVersionByChangelog logConfig@ChangelogConfig{..} = do
  versionedChanges <- getLevelOfChanges changelogChangelog changelogLevelHeaders
  case versionedChanges of
    Just lev -> generateVersion lev logConfig
    Nothing -> do
      warning $ "keeping current version since " <> showPath changelogChangelog <> " does not contain any new level headers or even entries."
      return Nothing

bumpVersions :: Bool -> ChangelogConfig -> Appl ()
bumpVersions upToDate config@ChangelogConfig{..} = do
  Options{..} <- ask
  if
    | not upToDate && not optForce ->
        failure $ "cannot bump versions because " <> format fp changelogChangelog <> " is out of date.\nUse --no-check to skip changelog checks.\nUse --force to force bump version."
    | not upToDate && optForce ->
        warning $ format fp changelogChangelog <> " is out of date. Bumping versions anyway due to --force."
    | otherwise -> (do
        newVersion <- case (optNoCheck, optChangeLevel) of
          (_, Just lev) -> generateVersion lev config
          (True, Nothing) ->  do
            failure "cannot infer new version from changelog because of --no-check.\nUse explicit --level CHANGE_LEVEL."
            return Nothing
          (False, Nothing) -> generateVersionByChangelog config

        case newVersion of
          Nothing -> return ()
          Just version -> case changelogVersionFiles of
            Just versionFiles -> do
              mapM_ (bumpPart version) versionFiles
              headChangelog version changelogChangelog
            Nothing -> warning "no files specified to bump versions in"
        ) `catch` (\(ex :: PatternMatchFail) -> failure (showText ex))
