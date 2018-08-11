{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Changelogged.Versions.Bump where

import           Prelude                     hiding (FilePath)
import           Turtle

import           Control.Exception           hiding (catch)
import qualified Control.Foldl               as Fold
import           Control.Monad.Catch

import           Data.Maybe                  (fromMaybe, listToMaybe)

import           Filesystem.Path.CurrentOS   (encodeString)
import           System.Console.ANSI         (Color (..))

import           Changelogged.Common
import           Changelogged.Pattern
import           Changelogged.Versions.Utils

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
  printf ("Version in " %fp% " : " %s%" -> ") (versionFilePath indicator) current
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
  Options{..} <- asks envOptions
  if (upToDate || optForce)
    then (do
      when optForce $ warning $ format fp changelogChangelog <> " is out of date. Bumping versions anyway due to --force."
      newVersion <- case optChangeLevel of
        Just lev -> generateVersion lev config
        Nothing  -> generateVersionByChangelog config

      case newVersion of
        Nothing -> return ()
        Just version -> case changelogVersionFiles of
          Just versionFiles -> do
            mapM_ (bumpPart version) versionFiles
            headChangelog version changelogChangelog
          Nothing -> warning "no files specified to bump versions in"
      ) `catch` (\(ex :: PatternMatchFail) -> failure (showText ex))
    else failure $ "cannot bump versions because " <> format fp changelogChangelog <> " is out of date.\n" <>
                   "Use --no-check to skip changelog checks.\n" <>
                   "Use --force to force bump version."
