{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Changelogged.Changelog.Check where

import           Prelude                        hiding (FilePath)
import           Turtle                         hiding (find, stderr, stdout)

import qualified Control.Foldl                  as Fold
import           Control.Monad                  (unless)

import           Changelogged.Changelog.Common
import           Changelogged.Changelog.Interactive
import           Changelogged.Changelog.Plain
import           Changelogged.Common
import           Changelogged.Pattern
import           Changelogged.Git (retrieveCommitMessage)

checkChangelog :: GitInfo -> ChangelogConfig -> Appl ()
checkChangelog gitInfo@GitInfo{..} config@ChangelogConfig{..} = do
  CommonOptions{..} <- gets (optionsCommon . envOptions)
  cmdOpts <- gets (optionsCmd . envOptions)
  info $ "looking for missing entries in " <> format fp changelogChangelog <> "\n"

  commitHashes <- map (fromJustCustom "Cannot find commit hash in git log entry" . hashMatch . lineToText)
    <$> fold (select gitHistory) Fold.list

  let listMisses = case cmdOpts of
        Right _ -> True
        Left ChangelogOptions{..} -> optListMisses
  unless listMisses $ info $ "You have entered interactiive session with Changelogged.\n"
      <> "You will be asked what to do with each suggested entry.\n"
      <> "You can:\n"
      <> "  1. Write entry to changelog (type w/write and press Enter, or simply press Enter)\n"
      <> "  2. Skip entry (type s/skip and press Enter)\n"
      <> "  3. Write entry and go into it's subchanges if it was merge commit (type e/expand and press Enter)\n"
      <> "  4. Ask changelogged to remind commit contents (type r/remind and press Enter). It's git show actually\n"
      <> "  5. Set changelogged to always ignore commit with such commit message (it will never appear in interactive session)\n"
      <> "     (type i/ignore and press Enter). It's git show actually\n"

  flags <- mapM (dealWithCommit gitInfo config) (map SHA1 commitHashes)
  if and flags
    then success $ showPath changelogChangelog <> " is up to date.\n"
                   <> "You can edit it manually now and arrange levels of changes if not yet.\n"
                   <> "To bump versions run changelogged bump-versions."
    else warning $ showPath changelogChangelog <> " does not mention all git history entries.\n"
                   <> "You can run changelogged to update it interactively.\n"
                   <> "Or you are still allowed to keep them missing and bump versions."

dealWithCommit :: GitInfo -> ChangelogConfig -> SHA1 -> Appl Bool
dealWithCommit GitInfo{..} ChangelogConfig{..} commitSHA = do
  CommonOptions{..} <- gets (optionsCommon . envOptions)
  cmdOpts <- gets (optionsCmd . envOptions)
  ignoreChangeReasoned <- sequence $
    [ commitNotWatched changelogWatchFiles commitSHA
    , allFilesIgnored changelogIgnoreFiles commitSHA
    , commitIgnored changelogIgnoreCommits commitSHA]
  if or ignoreChangeReasoned then return True else do
    commitIsPR <- fmap (PR . fromJustCustom "Cannot find commit hash in git log entry" . githubRefMatch . lineToText) <$>
        fold (grep githubRefGrep (grep (has (text (getSHA1 commitSHA))) (select gitHistory))) Fold.head
    commitMessage <- retrieveCommitMessage commitIsPR commitSHA
    case cmdOpts of
      Right _ -> plainDealWithEntry Commit{..} changelogChangelog
      Left ChangelogOptions{..} -> if optListMisses
        then plainDealWithEntry Commit{..} changelogChangelog
        else interactiveDealWithEntry gitRemoteUrl Commit{..} changelogChangelog
