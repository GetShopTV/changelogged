{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Changelogged.Changelog.Check where

import           Prelude                        hiding (FilePath)
import           Turtle                         hiding (find, stderr, stdout)

import qualified Control.Foldl                  as Fold

import           Changelogged.Changelog.Common
import           Changelogged.Changelog.Interactive
import           Changelogged.Changelog.Plain
import           Changelogged.Common
import           Changelogged.Pattern

checkChangelog :: GitInfo -> ChangelogConfig -> Appl ()
checkChangelog gitInfo@GitInfo{..} config@ChangelogConfig{..} = do
  Options{..} <- asks envOptions
  when optFromBC $ printf ("Checking "%fp%" from start of project\n") changelogChangelog
  info $ "looking for missing entries in " <> format fp changelogChangelog

  commitHashes <- map (fromJustCustom "Cannot find commit hash in git log entry" . hashMatch . lineToText)
    <$> fold (select gitHistory) Fold.list
  if (optListMisses || optAction == Just BumpVersions)
    then do
      flags <- mapM (dealWithCommit gitInfo config) (map SHA1 commitHashes)
      if and flags
        then success $ showPath changelogChangelog <> " is up to date.\n"
                       <> "You can edit it manually now and arrange levels of changes if not yet.\n"
                       <> "To bump versions run changelogged bump-versions."
        else warning $ showPath changelogChangelog <> " does not mention all git history entries.\n"
                       <> "You can run changelogged to update it interactively.\n"
                       <> "Or you are still allowed to keep them missing and bump versions."
    else mapM_ (dealWithCommit gitInfo config) (map SHA1 commitHashes)

dealWithCommit :: GitInfo -> ChangelogConfig -> SHA1 -> Appl Bool
dealWithCommit GitInfo{..} ChangelogConfig{..} commitSHA = do
  Options{..} <- asks envOptions
  ignoreChangeReasoned <- sequence $
    [ commitNotWatched changelogWatchFiles commitSHA
    , allFilesIgnored changelogIgnoreFiles commitSHA
    , commitIgnored changelogIgnoreCommits commitSHA]
  if or ignoreChangeReasoned then return True else do
    commitIsPR <- fmap (PR . fromJustCustom "Cannot find commit hash in git log entry" . githubRefMatch . lineToText) <$>
        fold (grep githubRefGrep (grep (has (text (getSHA1 commitSHA))) (select gitHistory))) Fold.head
    commitMessage <- retrieveCommitMessage commitIsPR commitSHA
    if optListMisses
      then plainDealWithEntry Commit{..} changelogChangelog
      else interactiveDealWithEntry gitRemoteUrl Commit{..} changelogChangelog
