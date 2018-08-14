{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
module Changelogged.Options
  ( Options(..),
    parseOptions
  ) where

import           Data.Char                      (toLower)
import           Data.List                      (intercalate)
import           Data.Monoid                    ((<>))
import           Data.String.Conversions        (cs)

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
-- >>> availableActions
-- [BumpVersions]
availableActions :: [Action]
availableActions = if (minBound :: Action) == maxBound
  then [minBound]
  else [minBound..maxBound]

-- >>> availableActionsStr
-- "'update-changelogs' or 'bump-versions'"
availableActionsStr :: String
availableActionsStr = prettyPossibleValues availableActions

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

readLevel :: ReadM Level
readLevel = eitherReader (r . map toLower)
  where
    r "app"   = Right App
    r "major" = Right Major
    r "minor" = Right Minor
    r "fix"   = Right Fix
    r "doc"   = Right Doc
    r lvl = Left $
         "Unknown level of changes: " <> show lvl <> ".\n"
      <> "Should be " <> availableLevelsStr <> ".\n"

readAction :: ReadM Action
readAction = eitherReader (r . map toLower)
  where
    r "bump-versions"      = Right BumpVersions
    r "bump-version"       = Right BumpVersions
    r cmd = Left $
         "Unknown command: " <> show cmd <> ".\n"
      <> "Should be " <> availableActionsStr <> ".\n"

readFilePath :: ReadM FilePath
readFilePath = eitherReader r
  where
    r filePathString = if valid $ fromText $ cs filePathString
      then Right (fromText $ cs filePathString)
      else Left ("Invalid file path " <> filePathString <> ".\n")

parser :: Parser Options
parser = Options
  <$> optional changeloggedAction
  <*> optional changesLevel
  <*> hiddenSwitch "list-misses" "List missing entries in simplest format with no expansion, don't modify anything."
  <*> hiddenSwitch "from-bc" "Look for missing changelog entries from the start of the project."
  <*> hiddenSwitch "no-colors" "Print all messages in standard terminal color."
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

    changeloggedAction = argument readAction $
         metavar "ACTION"
      <> completeWith ["bump-versions"]
      <> help ("argument form: bump-versions.")

    changesLevel = option readLevel $
         long "level"
      <> metavar "CHANGE_LEVEL"
      <> help (unlines
           [ "Level of changes (to override one inferred from changelog)."
           , "CHANGE_LEVEL can be " <> availableLevelsStr <> "."
           ])
      <> hidden

    targetChangelog = argument readFilePath $
         metavar "TARGET_CHANGELOG"
      <> help ("Path to target changelog.")

    configPath = option readFilePath $
         long "config"
      <> metavar "changelogged.yaml config file location"
      <> help ("Path to config file.")

-- | Parse command line options.
parseOptions :: IO Options
parseOptions = execParser $ info (helper <*> parser)
    ( fullDesc
   <> progDesc "Changelogged"
   <> header "Changelog Manager for Git Projects")
