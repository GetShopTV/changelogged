{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
module Changelogged.Options
  ( CommonOptions(..)
  , ChangelogOptions(..)
  , VersionOptions(..)
  , parseOptions
  ) where

import           Data.Char                      (toLower)
import           Data.List                      (intercalate)
import           Data.Monoid                    ((<>))
import           Data.String.Conversions        (cs)
import           Data.Text                      (Text)

import           Options.Applicative

import           Filesystem.Path.CurrentOS
import           Prelude                        hiding (FilePath)

import           Changelogged.Common.Types
import           Changelogged.Common.Utils.Pure

-- |
-- >>> availableLevels
-- [App,Major,Minor,Fix,Doc]
availableLevels :: [Level]
availableLevels = [minBound..maxBound]

-- |
-- >>> availableLevelsStr
-- "'app', 'major', 'minor', 'fix' or 'doc'"
availableLevelsStr :: String
availableLevelsStr = prettyPossibleValues availableLevels

prettyPossibleValues :: Show a => [a] -> String
prettyPossibleValues xs = case reverse xs of
  []  -> "none"
  [y] -> prettyValue y
  (y:ys) -> intercalate ", " (map prettyValue (reverse ys)) <> " or " <> prettyValue y
  where
    prettyValue v = "'" <> hyphenate (show v) <> "'"

readOptionalText :: ReadM (Maybe Text)
readOptionalText = eitherReader (r . map toLower)
  where
    r "init" = Right Nothing
    r txt   = Right (Just (cs txt))

readLevel :: ReadM Level
readLevel = eitherReader (r . map toLower)
  where
    r "application" = Right Application
    r "major"       = Right Major
    r "minor"       = Right Minor
    r "fix"         = Right Fix
    r "doc"         = Right Doc
    r lvl = Left $
         "Unknown level of changes: " <> show lvl <> ".\n"
      <> "Should be " <> availableLevelsStr <> ".\n"

readFilePath :: ReadM FilePath
readFilePath = eitherReader r
  where
    r filePathString = if valid $ fromText $ cs filePathString
      then Right (fromText $ cs filePathString)
      else Left ("Invalid file path " <> filePathString <> ".\n")

parserCommon :: Parser CommonOptions
parserCommon = CommonOptions
  <$> hiddenSwitch "no-colors" "Print all messages in standard terminal color."
  <*> longSwitch "dry-run" "Do not change files while running."
  <*> optional targetChangelog
  <*> optional configPath
  <*> hiddenSwitch "verbose" "Turn verbose mode on (useful for developers)."
  <*> hiddenSwitch "version" "Print version."
  where
    longSwitch name description = switch $
         long name
      <> help description

    hiddenSwitch name description = switch $
         long name
      <> help description
      <> hidden

    targetChangelog = argument readFilePath $
         metavar "TARGET_CHANGELOG"
      <> help ("Path to target changelog.")

    configPath = option readFilePath $
         long "config"
      <> metavar "changelogged.yaml config file location"
      <> help ("Path to config file.")

parserChangelog :: Parser (Either ChangelogOptions VersionOptions)
parserChangelog = fmap Left $ ChangelogOptions
  <$> hiddenSwitch "list-misses" "List missing entries in simplest format with no expansion, don't modify anything."
  <*> optional fromVersion
  where
    hiddenSwitch name description = switch $
         long name
      <> help description
      <> hidden

    fromVersion = option readOptionalText $
         long "from-version"
      <> metavar "CHECK_FROM_TAG"
      <> help (unlines
           [ "Tag or commit from which to check changelogs."
           ])
      <> hidden

parserVersion :: Parser (Either ChangelogOptions VersionOptions)
parserVersion = fmap Right $ VersionOptions
  <$> optional changesLevel
  where
    changesLevel = argument readLevel $
         metavar "CHANGE_LEVEL"
      <> completeWith (map (hyphenate . show) availableLevels)
      <> help (unlines
           [ "Level of changes (to override one inferred from changelog)."
           , "CHANGE_LEVEL can be " <> availableLevelsStr <> "."
           ])
      <> hidden

parserCommandOptions :: Parser (Either ChangelogOptions VersionOptions)
parserCommandOptions = subparser $
  command "run"              (info (helper <*> parserChangelog)
    ( fullDesc
   <> progDesc "Update changelogs")) <>
  command "bump-versions" (info (helper <*> parserVersion)
    ( fullDesc
   <> progDesc "Bump versions"))

-- | Parse command line options.
parseOptions :: IO Options
parseOptions = execParser $ info (helper <*> (Options <$> parserCommandOptions <*> parserCommon))
    ( fullDesc
   <> progDesc "Changelogged"
   <> header "Changelog Manager for Git Projects")
