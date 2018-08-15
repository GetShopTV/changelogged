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
  (ChangeloggedEnv Options{..} Config{..}) <- get
  case (optTargetChangelog optionsCommon) of
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
  CommonOptions{..} <- gets (optionsCommon . envOptions)
  cmdOpts <- gets (optionsCmd . envOptions)
  liftIO $ putStrLn ""
  changelogExists <- testfile changelogChangelog
  when (not changelogExists) $ do
    info (format fp changelogChangelog <> " does not exist. Creating an empty changelog.")
    touch changelogChangelog
  
  checkChangelog gitInfo config
  case cmdOpts of
    Right _ -> bumpVersions config
    Left _ -> return ()

-- | Extract latest history and origin link from git through temporary file and store it in 'GitInfo'.
loadGitInfo
  :: Maybe Text -- ^ Branch with version tags (@HEAD@ is used by default).
  -> Appl GitInfo
loadGitInfo branch = do
  cmdOpts <- gets (optionsCmd . envOptions)
  latestTag    <- loadGitLatestTag branch
  gitHistory   <- case cmdOpts of
    Left ChangelogOptions{..} -> case optFromVersion of
      Nothing -> loadGitHistory latestTag
      Just tag -> printf ("Checking changelogs from "%s%"\n") (ppTag tag) >> loadGitHistory tag
    Right _ -> loadGitHistory latestTag
  
  gitRemoteUrl <- loadGitRemoteUrl
  let gitLatestVersion = extractVersion latestTag
  return GitInfo {..}
  where
    extractVersion tag = case Text.dropWhile (not . isDigit) <$> tag of
      Just ver | not (Text.null ver) -> Just ver
      _                              -> Nothing

ppTag :: Maybe Text -> Text
ppTag Nothing = "start of the project"
ppTag (Just tag) = tag

-- | Pretty print known information about a Git project.
ppGitInfo :: GitInfo -> Text
ppGitInfo GitInfo{..} = Text.unlines
  [ "Git remote URL: " <> getLink gitRemoteUrl
  , "Latest release: " <> fromMaybe "<none>" gitLatestVersion
  , "Changes since last release: " <> Text.pack (show (length gitHistory))
  ]
