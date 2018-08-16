{-# LANGUAGE DeriveGeneric #-}
module Changelogged.Common.Types.Config where

import           Data.Text                        (Text)
import           GHC.Generics                     (Generic)

import qualified Filesystem.Path.CurrentOS        as Path

import           Changelogged.Common.Types.Common

data Config = Config
  { configChangelogs    :: [ChangelogConfig]
  , configBranch        :: Maybe Text
  , configEntryFormat   :: Maybe EntryFormat
  , configEditorCommand :: Maybe Text
  } deriving (Eq, Show, Generic)

data ChangelogConfig = ChangelogConfig
  { changelogChangelog     :: Path.FilePath
  , changelogWatchFiles    :: Maybe [Path.FilePath]
  , changelogIgnoreFiles   :: Maybe [Path.FilePath]
  , changelogIgnoreCommits :: Maybe [SHA1]
  , changelogVersionFiles  :: Maybe [VersionFile]
  } deriving (Eq, Show, Generic)

data VersionPattern = VersionPattern
  { versionPatternVariable  :: Text
  , versionPatternSeparator :: Text
  } deriving (Show, Eq, Generic)

data VersionFile = VersionFile
  { versionFilePath           :: Path.FilePath
  , versionFileVersionPattern :: VersionPattern
  } deriving (Show, Eq, Generic)
