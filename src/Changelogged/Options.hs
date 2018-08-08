{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Changelogged.Options
  ( module Control.Monad.Reader,

    Appl(..),
    runInAppl,
    Options(..),
    parseOptions
  ) where

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Reader

import Data.Aeson (ToJSON(..))
import Data.Char (toLower)
import Data.List (intercalate)
import Data.Monoid ((<>))
import Data.String.Conversions (cs)
import GHC.Generics (Generic)

import Options.Applicative
import qualified Turtle

import Filesystem.Path.CurrentOS (valid, fromText)

import Changelogged.Types
import Changelogged.Pure (hyphenate)

newtype Appl a = Appl { runAppl :: ReaderT Options IO a }
  deriving newtype (Functor, Applicative, Monad, MonadReader Options, MonadIO, MonadBase IO, MonadThrow, MonadCatch)

runInAppl :: Options -> Appl a -> IO a
runInAppl opts r = runReaderT (runAppl r) opts

-- |
-- >>> availableWarningFormats
-- [simple,suggest]
availableWarningFormats :: [WarningFormat]
availableWarningFormats = [minBound..maxBound]

-- |
-- >>> availableWarningFormatsStr
-- "'simple' or 'suggest'"
availableWarningFormatsStr :: String
availableWarningFormatsStr = prettyPossibleValues availableWarningFormats

readWarningFormat :: ReadM WarningFormat
readWarningFormat = eitherReader (r . map toLower)
  where
    r "simple"  = Right WarnSimple
    r "suggest" = Right WarnSuggest
    r fmt = Left $
         "Unknown warning format: " <> show fmt <> ".\n"
      <> "Use one of " <> availableWarningFormatsStr <> ".\n"

-- |
-- >>> availableLevels
-- [App,Major,Minor,Fix,Doc]
availableLevels :: [Level]
availableLevels = [minBound..maxBound]

-- |
-- >>> availableActions
-- [UpdateChangelogs,BumpVersions]
availableActions :: [Action]
availableActions = [minBound..maxBound]

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
    r "update-changelog"   = Right UpdateChangelogs
    r "update-changelogs"  = Right UpdateChangelogs
    r "bump-versions"      = Right BumpVersions
    r "bump-version"       = Right BumpVersions
    r cmd = Left $
         "Unknown command: " <> show cmd <> ".\n"
      <> "Should be " <> availableActionsStr <> ".\n"

readFilePath :: ReadM Turtle.FilePath
readFilePath = eitherReader r
  where
    r filePathString = if valid $ fromText $ cs filePathString
      then Right (fromText $ cs filePathString)
      else Left ("Invalid file path " <> filePathString <> ".\n")

parser :: Parser Options
parser = Options
  <$> optional changeloggedAction
  <*> warningFormat
  <*> optional changesLevel
  <*> hiddenSwitch "from-bc"
        "Look for missing changelog entries from the start of the project."
  <*> hiddenSwitch "force" "Bump versions ignoring possibly outdated changelogs. Usable with bump-versions only"
  <*> hiddenSwitch "no-check" "Do not check if changelogs have any missing entries."
  <*> hiddenSwitch "no-colors" "Print all messages in standard terminal color."
  <*> hiddenSwitch "expand-pr" "Do not expand PRs."
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
      <> help ("If present could be update-changelog or bump-versions.")

    changesLevel = option readLevel $
         long "level"
      <> metavar "CHANGE_LEVEL"
      <> help (unlines
           [ "Level of changes (to override one inferred from changelog)."
           , "CHANGE_LEVEL can be " <> availableLevelsStr <> "."
           ])
      <> hidden
    
    warningFormat = option readWarningFormat $
         long "format"
      <> metavar "FORMAT"
      <> help ("Format for missing changelog entry warnings. FORMAT can be " <> availableWarningFormatsStr <> ".")
      <> value WarnSimple
      <> showDefault
    
    targetChangelog = argument readFilePath $
         metavar "TARGET_CHANGELOG"
      <> help ("Path to target changelog.")
    
    configPath = option readFilePath $
         long "config"
      <> metavar "changelogged.yaml config file location"
      <> help ("Path to config file.")

welcome :: Turtle.Description
welcome = Turtle.Description "changelogged - Changelog Manager for Git Projects"

-- | Command line options for @changelogged@.
data Options = Options
  { -- | Command to execute.
    optAction          :: Maybe Action
    -- | Format for missing changelog entry warnings.
  , optFormat          :: WarningFormat
    -- | Level of changes (to override one inferred from changelogs).
  , optChangeLevel     :: Maybe Level
    -- | Look for missing changelog entries from the start of the project.
  , optFromBC          :: Bool
    -- | Bump versions ignoring possibly outdated changelogs.
  , optForce           :: Bool
    -- | Do not check if changelogs have any missing entries.
  , optNoCheck         :: Bool
    -- | Print all texts in standard terminal color.
  , optNoColors        :: Bool
    -- | Expand PRs while suggesting and writing to changelog.
  , optExpandPR        :: Bool
    -- | Run avoiding changes in files.
  , optDryRun          :: Bool
    -- | Check exactly one target changelog.
  , optTargetChangelog :: Maybe Turtle.FilePath
    -- | Use specified config file.
  , optConfigPath      :: Maybe Turtle.FilePath
    -- | Verbosity level.
  , optVerbose         :: Bool
    -- | Print version.
  , optVersion         :: Bool
  } deriving (Generic, Show, ToJSON)

instance ToJSON Turtle.FilePath where
  toJSON = toJSON . Turtle.format Turtle.fp

-- | Parse command line options.
parseOptions :: IO Options
parseOptions = Turtle.options welcome parser
