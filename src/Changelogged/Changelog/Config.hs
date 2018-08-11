{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module Changelogged.Changelog.Config where

import           Changelogged.Utils.Aeson (deriveJSON')
import           Data.Text                (Text)
import           GHC.Generics             (Generic)

-- | Default configuration.
--
-- >>> import qualified Data.ByteString.Char8 as BS8
-- >>> import qualified Data.Yaml as Yaml
-- >>> BS8.putStrLn (Yaml.encode defaultChangelogConfig)
-- changelog: ChangeLog.md
-- <BLANKLINE>
defaultChangelogConfig :: ChangelogConfig
defaultChangelogConfig = ChangelogConfig
  { changelogChangelog      = "ChangeLog.md"
  , changelogWatchFiles     = Nothing
  , changelogIgnoreFiles    = Nothing
  , changelogIgnoreCommits  = Nothing
  , changelogVersionFiles   = Nothing
  }

-- | Configuration for a single changelog.
data ChangelogConfig = ChangelogConfig
  { changelogChangelog     :: FilePath
    -- ^ Changelog file location.
  , changelogWatchFiles    :: Maybe [FilePath]
    -- ^ Changes to these files should be in this changelog (white list).
  , changelogIgnoreFiles   :: Maybe [FilePath]
    -- ^ Changes to these files are irrelevant to this changelog (black list).
  , changelogIgnoreCommits :: Maybe [GitCommit]
    -- ^ This specific Git commits are ignored.
  , changelogVersionFiles  :: Maybe [VersionFile]
    -- ^ These files contain version number to extract and bump.
  } deriving (Eq, Show, Generic)

-- | A version file configuration.
data VersionFile = VersionFile
  { versionFilePath           :: FilePath
    -- ^ File location where a version is specified.
  , versionFileVersionPattern :: Maybe Pattern
    -- ^ How to find a version in the file.
  } deriving (Show, Eq, Generic)

-- | A regex pattern.
type Pattern = Text

-- | Git commit SHA.
type GitCommit = Text

deriveJSON' ''ChangelogConfig
deriveJSON' ''VersionFile
