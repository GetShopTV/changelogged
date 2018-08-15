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

levelPrompt :: Appl (Maybe Level)
levelPrompt = do
  coloredPrint Yellow $ "Changelog does not contain any new version level labels.\n"
                     <> "You can specify level of changes explicitly or press Enter to miss bumping versions.\n"
  go
  where go = do
          coloredPrint Cyan "(↵/app↵/major↵/minor↵/fix↵/doc↵):  \n"
          answer <- liftIO getLine
          case answer of
            "" -> return Nothing
            "App" -> return (Just App)
            "app" -> return (Just App)
            "Major" -> return (Just Major)
            "major" -> return (Just Major)
            "Minor" -> return (Just Minor)
            "minor" -> return (Just Minor)
            "Fix" -> return (Just Fix)
            "fix" -> return (Just Fix)
            "Doc" -> return (Just Doc)
            "doc" -> return (Just Doc)
            _ -> do
              liftIO $ putStrLn "Cannot parse level. Please repeat."
              go

-- |Infer new local version.
generateVersionByChangelog :: ChangelogConfig -> Appl (Maybe Version)
generateVersionByChangelog logConfig@ChangelogConfig{..} = do
  versionedChanges <- getLevelOfChanges changelogChangelog changelogLevelHeaders
  case versionedChanges of
    Just lev -> generateVersion lev logConfig
    Nothing -> do
      levelm <- levelPrompt
      case levelm of
        Nothing -> do
          warning $ "keeping current version since " <> showPath changelogChangelog <> " does not contain any new level headers or even entries."
          return Nothing
        Just level -> generateVersion level logConfig

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
