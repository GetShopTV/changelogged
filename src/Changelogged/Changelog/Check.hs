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

checkChangelog :: GitInfo -> ChangelogConfig -> Appl Bool
checkChangelog gitInfo@GitInfo{..} config@ChangelogConfig{..} = do
  Options{..} <- asks envOptions
  when optFromBC $ printf ("Checking "%fp%" from start of project\n") changelogChangelog
  info $ "looking for missing entries in " <> format fp changelogChangelog

  commitHashes <- map (fromJustCustom "Cannot find commit hash in git log entry" . hashMatch . lineToText)
    <$> fold (select gitHistory) Fold.list
  upToDate <- if optListMisses
    then do
      flags <- mapM (dealWithCommit gitInfo config) (map SHA1 commitHashes)
      if and flags
        then success (showPath changelogChangelog <> " is up to date.\n" <> "You can run bump-versions to bump versions for it.")
        else do
          warning $ showPath changelogChangelog <> " is out of date." <> "\nRun changelogged to update it interactively."
      return $ and flags
    else mapM_ (dealWithCommit gitInfo config) (map SHA1 commitHashes) >> return True

  return upToDate

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
