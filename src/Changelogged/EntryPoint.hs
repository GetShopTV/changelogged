{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Changelogged.EntryPoint
  ( processChangelogs
  , loadGitInfo
  , ppGitInfo
  ) where

import           Control.Monad                (unless)

import qualified System.Process               as Proc

import           Prelude                      hiding (FilePath)
import           Turtle                       hiding (find)

import           Data.Char                    (isDigit)
import           Data.List                    (find)
import           Data.Maybe                   (fromMaybe)
import           Data.String.Conversions      (cs)
import qualified Data.Text                    as Text

import           Filesystem.Path.CurrentOS    (encodeString)

import           Changelogged.Changelog.Check
import           Changelogged.Common
import           Changelogged.Git
import           Changelogged.Versions.Bump

processChangelogs :: GitInfo -> Appl ()
processChangelogs gitInfo = do
  (ChangeloggedEnv Options{..} Config{..}) <- get
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
  Options{..} <- gets envOptions
  editorCommand <- gets (configEditorCommand . envConfig)
  liftIO $ putStrLn ""
  changelogExists <- testfile changelogChangelog
  when (not changelogExists) $ do
    info (format fp changelogChangelog <> " does not exist. Creating an empty changelog.")
    touch changelogChangelog
  
  checkChangelog gitInfo config
  unless optListMisses $ do
    enableEditor editorCommand changelogChangelog
    bumpVersions config

enableEditor :: Maybe Text -> FilePath -> Appl ()
enableEditor cmd file = do
  let editorCmd =
        case cmd of
          Nothing -> "vim"
          Just ed -> ed
  (_,_,_,waiter) <- liftIO $ Proc.createProcess Proc.CreateProcess
    { Proc.cmdspec = (Proc.ShellCommand (cs editorCmd <> " " <> encodeString file))
    , Proc.cwd = Nothing
    , Proc.env = Nothing
    , Proc.std_in = Proc.Inherit
    , Proc.std_out = Proc.Inherit
    , Proc.std_err = Proc.Inherit
    , Proc.close_fds = True
    , Proc.create_group = False
    , Proc.delegate_ctlc = True
    , Proc.detach_console = True
    , Proc.create_new_console = True
    , Proc.new_session = True
    , Proc.child_group = Nothing
    , Proc.child_user = Nothing
    , Proc.use_process_jobs = False
    }
  void . liftIO . Proc.waitForProcess $ waiter

-- | Extract latest history and origin link from git through temporary file and store it in 'GitInfo'.
loadGitInfo
  :: Maybe Text -- ^ Branch with version tags (@HEAD@ is used by default).
  -> Appl GitInfo
loadGitInfo branch = do
  fromTag      <- gets (optFromVersion . envOptions)
  latestTag    <- loadGitLatestTag branch
  gitHistory   <- loadGitHistory (case fromTag of
    Nothing -> latestTag
    Just tag -> tag)
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
