{-# LANGUAGE DeriveGeneric #-}
module Changelogged.Common.Types.Options where

import           Data.Text                        (Text)
import           GHC.Generics                     (Generic)

import qualified Filesystem.Path.CurrentOS        as Path

-- | Command line options for @changelogged@.
data Options = Options
  { -- | Only display report on changelog misses.
    optListMisses      :: Bool
    -- | Check changelogs from specified version tag.
  , optFromVersion     :: Maybe Text
    -- | Check changelogs from the very start.
  , optFromBeginning   :: Bool
    -- | Print all texts in standard terminal color.
  , optNoColors        :: Bool
    -- | Run avoiding changes in files.
  , optDryRun          :: Bool
    -- | Say no to all question
  , optNoPrompts       :: Bool
    -- | Dump default config into file.
  , optDumpConfig      :: Bool
    -- | Check exactly one target changelog.
  , optTargetChangelog :: Maybe Path.FilePath
    -- | Use specified config file.
  , optConfigPath      :: Maybe Path.FilePath
    -- | Verbosity level.
  , optVerbose         :: Bool
    -- | Print version.
  , optVersion         :: Bool
  } deriving (Generic, Show)
