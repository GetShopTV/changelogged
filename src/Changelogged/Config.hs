module Changelogged.Config where

import Data.Either.Combinators (rightToMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Yaml as Yaml

import qualified Turtle

import Changelogged.Common
import Changelogged.Aeson ()

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

loadConfig :: FilePath -> IO (Maybe Config)
loadConfig path = Yaml.decodeFileEither path >>= return . rightToMaybe

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
