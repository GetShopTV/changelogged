{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
module Changelogged.Options
  ( Options(..),
    parseOptions
  ) where

import           Data.Monoid                    ((<>))
import           Data.String.Conversions        (cs)

import           Options.Applicative

import           Filesystem.Path.CurrentOS
import           Prelude                        hiding (FilePath)

import           Changelogged.Common.Types

readFilePath :: ReadM FilePath
readFilePath = eitherReader r
  where
    r filePathString = if valid $ fromText $ cs filePathString
      then Right (fromText $ cs filePathString)
      else Left ("Invalid file path " <> filePathString <> ".\n")

parser :: Parser Options
parser = Options
  <$> hiddenSwitch "list-misses" "List missing entries in simplest format with no expansion, don't modify anything."
  <*> optional fromVersion
  <*> hiddenSwitch "from-beginning" "Check all changelogs from start of the project."
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

    fromVersion = option str $
         long "from-version"
      <> metavar "CHECK_FROM_TAG"
      <> help (unlines
           [ "Tag or commit from which to check changelogs."
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
