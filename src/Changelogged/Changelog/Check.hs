{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Changelogged.Changelog.Check where

import           Prelude                        hiding (FilePath)
import           Turtle                         hiding (find, stderr, stdout)

import qualified Control.Foldl                  as Fold
import           Control.Monad                  (when)

import           Data.Maybe                     (catMaybes)

import           Changelogged.Changelog.Common
import           Changelogged.Changelog.Interactive
import           Changelogged.Changelog.Dry
import           Changelogged.Common
import           Changelogged.Pattern
import           Changelogged.Git (retrieveCommitMessage, parseHostingType)

checkChangelog :: GitInfo -> ChangelogConfig -> Appl ()
checkChangelog gitInfo@GitInfo{..} config@ChangelogConfig{..} = do
  Options{..} <- gets envOptions
  when optFromBeginning $ printf ("Checking "%fp%" from start of the project\n") changelogChangelog
  case optFromVersion of
    Nothing -> printf ("Checking "%fp%" from latest version\n") changelogChangelog
    Just tag -> printf ("Checking "%fp%" from "%s%"\n") changelogChangelog tag
  info $ "looking for missing entries in " <> format fp changelogChangelog <> "\n"

  commitHashes <- map (fromJustCustom "Cannot find commit hash in git log entry" . hashMatch . lineToText)
    <$> fold (select gitHistory) Fold.list

  checkableCommits <- catMaybes <$> mapM (extractCommitMetadata gitInfo config) (map SHA1 commitHashes)

  upToDate <- listEntries changelogChangelog checkableCommits
  if optListMisses 
    then if upToDate
      then success $ showPath changelogChangelog <> " is up to date.\n"
                    <> "You can use changelogged to bump versions.\n"
      else warning $ showPath changelogChangelog <> " does not mention all git history entries.\n"
                    <> "You can run changelogged to update it interactively and bump versions.\n"
    else do
      interactiveMode <- promptGoInteractive
      (if interactiveMode
              then interactiveWalk gitRemoteUrl changelogChangelog 
              else simpleWalk gitRemoteUrl changelogChangelog) $
            checkableCommits
      success $ showPath changelogChangelog <> " is updated.\n"

extractCommitMetadata :: GitInfo -> ChangelogConfig -> SHA1 -> Appl (Maybe Commit)
extractCommitMetadata GitInfo{..} ChangelogConfig{..} commitSHA = do
  let hosting = parseHostingType gitRemoteUrl
  ignoreChangeReasoned <- sequence $
    [ commitNotWatched changelogWatchFiles commitSHA
    , allFilesIgnored changelogIgnoreFiles commitSHA
    , commitIgnored changelogIgnoreCommits commitSHA]
  if or ignoreChangeReasoned
    then return Nothing 
    else do
      -- FIXME: impossible.
      commitIsPR <- fmap (PR . fromJustCustom "Cannot find commit hash in git log entry" . refMatch hosting . lineToText) <$>
          fold (grep (refGrep hosting) (grep (has (text (getSHA1 commitSHA))) (select gitHistory))) Fold.head
      commitMessage <- retrieveCommitMessage commitIsPR commitSHA
      return $ Just Commit{..}
