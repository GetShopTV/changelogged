{-# LANGUAGE DeriveGeneric #-}
module Changelogged.Common.Types.Options where

import           Data.Text                        (Text)
import           GHC.Generics                     (Generic)

import           Changelogged.Common.Types.Common

import qualified Filesystem.Path.CurrentOS        as Path

data Options = Options
  { optionsCmd    :: Either ChangelogOptions VersionOptions
  , optionsCommon :: CommonOptions
  } deriving (Generic, Show)

-- | Command line options for @changelogged@.
data CommonOptions = CommonOptions
  { -- | Print all texts in standard terminal color.
    optNoColors        :: Bool
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

-- | Command line options for @changelogged@.
data ChangelogOptions = ChangelogOptions
  { -- | Only display report on changelog misses.
    optListMisses      :: Bool
    -- | Check changelogs from specified version tag or from the very start.
  , optFromVersion     :: Maybe (Maybe Text)
  } deriving (Generic, Show)

-- | Command line options for @changelogged@.
data VersionOptions = VersionOptions
  { -- | Level of changes (to override one inferred from changelogs).
    optChangeLevel     :: Maybe Level
  } deriving (Generic, Show)
