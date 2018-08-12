{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Changelogged.Config where

import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Yaml as Yaml

import qualified Turtle

import           Changelogged.Aeson      ()
import           Changelogged.Common

defaultLevelHeaders :: LevelHeaders
defaultLevelHeaders = LevelHeaders
  { levelHeadersApp   = Just "* App"
  , levelHeadersMajor = Just "* Major"
  , levelHeadersMinor = Just "* Minor"
  , levelHeadersFix   = Just "* Fix"
  , levelHeadersDoc   = Just "* Doc"
  }

defaultConfig :: Config
defaultConfig = Config
  { configChangelogs    = pure ChangelogConfig
      { changelogChangelog     = "ChangeLog.md"
      , changelogLevelHeaders  = defaultLevelHeaders
      , changelogWatchFiles    = Nothing  -- watch everything
      , changelogIgnoreFiles   = Just ["ChangeLog.md"]
      , changelogIgnoreCommits = Nothing
      , changelogVersionFiles  = Just [VersionFile "package.yaml" (VersionPattern "version" ":")]
      }
  , configBranch = Nothing
  , configEntryFormat = Nothing
  }

loadConfig :: FilePath -> IO (Either Yaml.ParseException Config)
loadConfig path = Yaml.decodeFileEither path >>= mapM decodeCfgPathWildcards

decodeCfgPathWildcards :: Config -> IO Config
decodeCfgPathWildcards cfg'@Config{..} = do
  expandedConfigChangelogs <- mapM expandWildcards configChangelogs
  let changelogs = map changelogChangelog configChangelogs
      -- Ignore all changelogs by default.
      cfg = cfg' {configChangelogs = map (\cc -> cc {changelogIgnoreFiles = Just changelogs <> changelogIgnoreFiles cc}) configChangelogs}
  return $ cfg { configChangelogs = expandedConfigChangelogs }
  where
    expandWildcards cc@ChangelogConfig{..} = do
      expandedIgnoreFiles <- mapM decodePathWildcards changelogIgnoreFiles
      expandedWatchFiles <- mapM decodePathWildcards changelogWatchFiles
      return cc
        { changelogIgnoreFiles = expandedIgnoreFiles
        , changelogWatchFiles = expandedWatchFiles
        }

ppConfig :: Config -> Text
ppConfig Config{..} = mconcat
  [ "Main branch (with version tags)" ?: configBranch
  , "Format of inferred changelog entries" ?: (getEntryFormat <$> configEntryFormat)
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
