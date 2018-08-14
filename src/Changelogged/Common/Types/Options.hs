{-# LANGUAGE DeriveGeneric #-}
module Changelogged.Common.Types.Options where

import           Data.Text                        (Text)
import           GHC.Generics                     (Generic)

import           Changelogged.Common.Types.Common

import qualified Filesystem.Path.CurrentOS        as Path

-- | Command line options for @changelogged@.
data Options = Options
  { -- | Command to execute.
    optAction          :: Maybe Action
    -- | Level of changes (to override one inferred from changelogs).
  , optChangeLevel     :: Maybe Level
    -- | Only display report on changelog misses.
  , optListMisses      :: Bool
    -- | Check changelogs from specified version tag or from the very start.
  , optFromVersion     :: Maybe (Maybe Text)
    -- | Print all texts in standard terminal color.
  , optNoColors        :: Bool
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
