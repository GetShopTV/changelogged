{-# LANGUAGE DeriveGeneric #-}
module Changelogged.Common.Types.Config where

import Data.Text (Text)
import GHC.Generics (Generic)

import qualified Filesystem.Path.CurrentOS as Path

import Changelogged.Common.Types.Common

data Config = Config
  { configChangelogs    :: [ChangelogConfig]
  , configBranch        :: Maybe Text
  , configEntryFormat   :: Maybe EntryFormat
  } deriving (Eq, Show, Generic)

data LevelHeaders = LevelHeaders
  { levelHeadersApp   :: Maybe Text
  , levelHeadersMajor :: Maybe Text
  , levelHeadersMinor :: Maybe Text
  , levelHeadersFix   :: Maybe Text
  , levelHeadersDoc   :: Maybe Text
  } deriving (Eq, Show, Generic)

data ChangelogConfig = ChangelogConfig
  { changelogChangelog     :: Path.FilePath
  , changelogLevelHeaders  :: LevelHeaders
  , changelogWatchFiles    :: Maybe [Path.FilePath]
  , changelogIgnoreFiles   :: Maybe [Path.FilePath]
  , changelogIgnoreCommits :: Maybe [Text]
  , changelogVersionFiles  :: Maybe [VersionFile]
  } deriving (Eq, Show, Generic)

data VersionPattern = VersionPattern
  { versionPatternVariable  :: Text
  , versionPatternSeparator :: Text
  } deriving (Show, Eq, Generic)

data VersionFile = VersionFile
  { versionFilePath :: Path.FilePath
  , versionFileVersionPattern :: VersionPattern
  } deriving (Show, Eq, Generic)
