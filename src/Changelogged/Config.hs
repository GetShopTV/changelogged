{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Changelogged.Config where

import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Yaml as Yaml

import qualified Turtle

import           Changelogged.Aeson      ()
import           Changelogged.Common

defaultConfig :: Config
defaultConfig = Config
  { configChangelogs    = pure ChangelogConfig
      { changelogChangelog     = "ChangeLog.md"
      , changelogLevelHeaders  = Just defaultLevelHeaders
      , changelogWatchFiles    = Nothing  -- watch everything
      , changelogIgnoreFiles   = Just ["ChangeLog.md"]
      , changelogIgnoreCommits = Nothing
      , changelogVersionFiles  = Just [VersionFile "package.yaml" (VersionPattern "version" ":")]
      }
  , configBranch = Nothing
  , configEntryFormat = Nothing
  , configEditorCommand = Just "vim"
  }

addCommitToIgnored :: SHA1 -> Turtle.FilePath -> Appl ()
addCommitToIgnored hash changelog = do
  ChangeloggedEnv opts@Options{..} cfg'@Config{..} <- get
  let configPath = fromMaybe ".changelogged.yaml" (Text.unpack . showPath <$> optConfigPath)
      newIgnoreCommits cc = Just [hash] <> changelogIgnoreCommits cc
      replaceElem =
        (\(first, as) -> first <> ((head as) {changelogIgnoreCommits = newIgnoreCommits (head as)}: tail as))
        . break (\cconf -> changelogChangelog cconf == changelog)
      cfg = cfg' {configChangelogs = replaceElem configChangelogs}
  put $ ChangeloggedEnv opts cfg
  liftIO $ Yaml.encodeFile configPath cfg

loadConfig :: FilePath -> IO (Either Yaml.ParseException Config)
loadConfig path = Yaml.decodeFileEither path >>= mapM adjustConfig

adjustConfig :: Config -> IO Config
adjustConfig cfg'@Config{..} = do
  let changelogs = map changelogChangelog configChangelogs
      -- Ignore all changelogs by default.
      cfg = cfg' {configChangelogs = map (\cc -> cc {changelogIgnoreFiles = Just changelogs <> changelogIgnoreFiles cc}) configChangelogs}
  return cfg

ppConfig :: Config -> Text
ppConfig Config{..} = mconcat
  [ "Main branch (with version tags)" ?: configBranch
  , "Format of changelog entries" ?: (getEntryFormat <$> configEntryFormat)
  , "Editor" ?: configEditorCommand
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
