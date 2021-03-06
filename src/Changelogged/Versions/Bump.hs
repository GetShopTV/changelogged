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
    Nothing -> error "No version files specified for changelog."
    Just versionFiles -> do
      versions <- mapM (generateVersionForFile lev) versionFiles
      return (listToMaybe versions)

askGenerateVersion :: ChangelogConfig -> Appl (Maybe Version)
askGenerateVersion logConfig = do
  level <- levelPrompt
  generateVersion level logConfig

-- |Infer new local version.
generateVersionByChangelog :: ChangelogConfig -> Appl (Maybe Version)
generateVersionByChangelog logConfig@ChangelogConfig{..} = do
  versionedChanges <- predictLevelOfChanges changelogChangelog
  case versionedChanges of
    Just level -> do
      acceptVersion <- promptAcceptPredictedVersion level
      if acceptVersion
        then generateVersion level logConfig
        else askGenerateVersion logConfig
    Nothing -> askGenerateVersion logConfig

bumpVersions :: ChangelogConfig -> Appl ()
bumpVersions config@ChangelogConfig{..} = flip catch (\(ex :: PatternMatchFail) -> failure (showText ex)) $ do
  do
    newVersion <- generateVersionByChangelog config

    case newVersion of
      Nothing -> return ()
      Just version -> case changelogVersionFiles of
        Just versionFiles -> do
          mapM_ (bumpPart version) versionFiles
          headChangelog version changelogChangelog
        Nothing -> warning "no files specified to bump versions in"
