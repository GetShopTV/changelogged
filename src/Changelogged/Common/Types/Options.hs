{-# LANGUAGE DeriveGeneric #-}
module Changelogged.Common.Types.Options where

import GHC.Generics (Generic)

import Changelogged.Common.Types.Common

import qualified Filesystem.Path.CurrentOS as Path

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
  , optTargetChangelog :: Maybe Path.FilePath
    -- | Use specified config file.
  , optConfigPath      :: Maybe Path.FilePath
    -- | Verbosity level.
  , optVerbose         :: Bool
    -- | Print version.
  , optVersion         :: Bool
  } deriving (Generic, Show)