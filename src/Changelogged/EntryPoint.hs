{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Changelogged.EntryPoint
  ( processChangelogs
  , loadGitInfo
  , ppGitInfo
  ) where

import           Prelude                      hiding (FilePath)
import           Turtle                       hiding (find)

import           Data.Char                    (isDigit)
import           Data.List                    (find)
import           Data.Maybe                   (fromMaybe)
import qualified Data.Text                    as Text

import           Changelogged.Changelog.Check
import           Changelogged.Common
import           Changelogged.Git
import           Changelogged.Versions.Bump

processChangelogs :: GitInfo -> Appl ()
processChangelogs gitInfo = do
  (ChangeloggedEnv Options{..} Config{..}) <- ask
  case optTargetChangelog of
    Nothing -> if null configChangelogs
      then failure "You have empty configuration file"
      else mapM_ (processChangelog gitInfo) configChangelogs
    Just changelogPath -> do
      case lookupChangelog changelogPath configChangelogs of
        Just changelog -> processChangelog gitInfo changelog
        Nothing -> failure $ "Given target changelog " <> format fp changelogPath <> " is missed in config or mistyped."
      where
        lookupChangelog path changelogs = find (\entry -> changelogChangelog entry == path) changelogs

processChangelog :: GitInfo -> ChangelogConfig -> Appl ()
processChangelog gitInfo config@ChangelogConfig{..} = do
  Options{..} <- asks envOptions
  liftIO $ putStrLn ""
  info $ "processing " <> format fp changelogChangelog
  changelogExists <- testfile changelogChangelog
  when (not changelogExists) $ do
    info (format fp changelogChangelog <> " does not exist. Creating an empty changelog.")
    touch changelogChangelog

  upToDate <- checkChangelog gitInfo config
  case optAction of
    Just BumpVersions -> bumpVersions upToDate config
    Nothing -> return ()

-- | Extract latest history and origin link from git through temporary file and store it in 'GitInfo'.
loadGitInfo
  :: Maybe Text -- ^ Branch with version tags (@HEAD@ is used by default).
  -> Appl GitInfo
loadGitInfo branch = do
  entireHistory <- asks (optFromBC . envOptions)
  latestTag    <- loadGitLatestTag branch
  gitHistory   <- loadGitHistory (if entireHistory then Nothing else latestTag)
  gitRemoteUrl <- loadGitRemoteUrl
  let gitLatestVersion = extractVersion latestTag
  return GitInfo {..}
  where
    extractVersion tag = case Text.dropWhile (not . isDigit) <$> tag of
      Just ver | not (Text.null ver) -> Just ver
      _                              -> Nothing

-- | Pretty print known information about a Git project.
ppGitInfo :: GitInfo -> Text
ppGitInfo GitInfo{..} = Text.unlines
  [ "Git remote URL: " <> getLink gitRemoteUrl
  , "Latest release: " <> fromMaybe "<none>" gitLatestVersion
  , "Changes since last release: " <> Text.pack (show (length gitHistory))
  ]
