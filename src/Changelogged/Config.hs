{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Changelogged.Config where

import Data.Aeson.TH
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Yaml as Yaml

import qualified Turtle
import GHC.Generics

import Changelogged.Options
import Changelogged.Types ()
import Changelogged.Utils
import Changelogged.Pure

data Config = Config
  { configChangelogs    :: [ChangelogConfig]
  , configIgnoreCommits :: Maybe [Text]
  , configBranch        :: Maybe Text
  } deriving (Eq, Show, Generic)

data LevelHeaders = LevelHeaders
  { levelHeadersApp   :: Maybe Text
  , levelHeadersMajor :: Maybe Text
  , levelHeadersMinor :: Maybe Text
  , levelHeadersFix   :: Maybe Text
  , levelHeadersDoc   :: Maybe Text
  } deriving (Eq, Show, Generic)

data ChangelogConfig = ChangelogConfig
  { changelogChangelog    :: Turtle.FilePath
  , changelogLevelHeaders :: LevelHeaders
  , changelogWatchFiles   :: Maybe [Turtle.FilePath]
  , changelogIgnoreFiles  :: Maybe [Turtle.FilePath]
  , changelogVersionFiles :: Maybe [VersionFile]
  } deriving (Eq, Show, Generic)

data VersionPattern = VersionPattern
  { versionPatternVariable  :: Text
  , versionPatternSeparator :: Text
  } deriving (Show, Eq, Generic)

data VersionFile = VersionFile
  { versionFilePath :: Turtle.FilePath
  , versionFileVersionPattern :: VersionPattern
  } deriving (Show, Eq, Generic)

defaultLevelHeaders :: LevelHeaders
defaultLevelHeaders = LevelHeaders
  { levelHeadersApp = Just "* App"
  , levelHeadersMajor = Just "* Major"
  , levelHeadersMinor = Just "* Minor"
  , levelHeadersFix = Just "* Fix"
  , levelHeadersDoc = Just "* Doc"
  }

defaultConfig :: Config
defaultConfig = Config
  { configChangelogs    = pure ChangelogConfig
      { changelogChangelog    = "ChangeLog.md"
      , changelogLevelHeaders = defaultLevelHeaders
      , changelogWatchFiles   = Nothing  -- watch everything
      , changelogIgnoreFiles  = Nothing  -- ignore nothing
      , changelogVersionFiles = Just [VersionFile "package.yaml" (VersionPattern "version" ":")]
      }
  , configIgnoreCommits = Nothing
  , configBranch = Nothing
  }

loadConfig :: FilePath -> Appl (Maybe Config)
loadConfig path = do
  ms <- liftIO $ Yaml.decodeFileEither path
  case ms of
    Left _wrong -> return Nothing
    Right paths -> debugYaml "config:" paths >> return (Just paths)

ppConfig :: Config -> Text
ppConfig Config{..} = mconcat
  [ "Main branch (with version tags)" ?: configBranch
  , "Ignored commits" ?: (Text.pack . show . length <$> configIgnoreCommits)
  , "Changelogs" !: formatItems Turtle.fp (map changelogChangelog configChangelogs)
  ]
  where
    formatItems fmt
      = ("\n" <>)
      . Text.unlines
      . map (\x -> "- " <> Turtle.format fmt x)

    name !: val = name <> ": " <> val <> "\n"

    _    ?: Nothing = ""
    name ?: Just val = name !: val

deriveJSON (jsonDerivingModifier "VersionPattern") ''VersionPattern
deriveJSON (jsonDerivingModifier "VersionFile") ''VersionFile
deriveJSON (jsonDerivingModifier "LevelHeaders") ''LevelHeaders
deriveJSON (jsonDerivingModifier "Changelog") ''ChangelogConfig
deriveJSON (jsonDerivingModifier "Config") ''Config
