{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Changelogged.Config where

import Data.Aeson
import Data.Aeson.TH
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Yaml as Yaml

import qualified Turtle
import GHC.Generics

import Changelogged.Options
import Changelogged.Types ()
import Changelogged.Pure (toSnakeCase)

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

data VersionFile = VersionFile
  { versionFilePath :: Turtle.FilePath
  , versionFileVersionPattern :: Text
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
      , changelogVersionFiles = Just [VersionFile "package.yaml" "version:"]
      }
  , configIgnoreCommits = Nothing
  , configBranch = Nothing
  }

loadConfig :: FilePath -> Appl (Maybe Config)
loadConfig path = do
  ms <- liftIO $ Yaml.decodeFileEither path
  liftIO $ print ms
  return $ case ms of
    Left _wrong -> Nothing
    Right paths -> Just paths

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

deriveFromJSON (defaultOptions {
  fieldLabelModifier = toSnakeCase . drop (length ("VersionFile"::String))
  }) ''VersionFile
deriveFromJSON (defaultOptions {
  fieldLabelModifier = toSnakeCase . drop (length ("LevelHeaders"::String))
  }) ''LevelHeaders
deriveFromJSON (defaultOptions {
  fieldLabelModifier = toSnakeCase . drop (length ("Changelog"::String))
  }) ''ChangelogConfig
deriveFromJSON (defaultOptions {
  fieldLabelModifier = toSnakeCase . drop (length ("Config"::String))
  }) ''Config
