{-# LANGUAGE ScopedTypeVariables #-}
module Changelogged.Changelog.Check where

import Turtle hiding (stdout, stderr, find)
import Prelude hiding (FilePath)
import Data.Foldable (asum)

import qualified Control.Foldl as Fold

import Changelogged.Changelog.Compose
import Changelogged.Common
import Changelogged.Pattern

checkChangelog :: GitInfo -> ChangelogConfig -> Appl Bool
checkChangelog gitInfo@GitInfo{..} config@ChangelogConfig{..} = do
  Options{..} <- asks envOptions
  upToDate <- if optNoCheck
    then do
      warning $ "skipping checks for " <> format fp changelogChangelog <> " (due to --no-check)."
      return True
    else do
      when optFromBC $ printf ("Checking "%fp%" from start of project\n") changelogChangelog
      info $ "looking for missing entries in " <> format fp changelogChangelog
  
      commitHashes <- map (fromJustCustom "Cannot find commit hash in git log entry" . hashMatch . lineToText)
        <$> fold (select gitHistory) Fold.list
      flags <- mapM (checkCommits gitInfo config) (map SHA1 commitHashes)
      return $ and flags

  if upToDate
    then success (showPath changelogChangelog <> " is up to date.\n" <> "You can run bump-versions to bump versions.")
    else do
      warning $ showPath changelogChangelog <> " is out of date." <>
        if optAction == Just UpdateChangelogs
          then ""
          else "\nUse update-changelog to add missing changelog entries automatically."
  
  return upToDate

checkCommits :: GitInfo -> ChangelogConfig -> SHA1 -> Appl Bool
checkCommits GitInfo{..} ChangelogConfig{..} commitSHA = do
  ignoreChangeReasoned <- sequence $
    [ commitNotWatched changelogWatchFiles commitSHA
    , allFilesIgnored changelogIgnoreFiles commitSHA
    , commitIgnored changelogIgnoreCommits commitSHA]
  if or ignoreChangeReasoned then return True else do
    commitIsPR <- fmap (PR . fromJustCustom "Cannot find commit hash in git log entry" . githubRefMatch . lineToText) <$>
        fold (grep githubRefGrep (grep (has (text (getSHA1 commitSHA))) (select gitHistory))) Fold.head
    commitMessage <- retrieveCommitMessage commitIsPR commitSHA
    changelogIsUp gitRemoteUrl Commit{..} changelogChangelog

allFilesIgnored :: Maybe [FilePath] -> SHA1 -> Appl Bool
allFilesIgnored Nothing _ = return False
allFilesIgnored (Just files) (SHA1 commit) = do
  affectedFiles <- fold (inproc "git" ["diff-tree", "--name-only", "--no-commit-id", "-m", commit] empty) Fold.list
  return . and $ map (flip elem files . fromText . lineToText) affectedFiles

commitNotWatched :: Maybe [FilePath] -> SHA1 -> Appl Bool
commitNotWatched Nothing _ = return False
commitNotWatched (Just files) (SHA1 commit) = fold
  (grep (asum (map (text . showPath) files))
    (inproc "git" ["diff-tree", "--name-only", "--no-commit-id", "-m", commit] empty))
  Fold.null

commitIgnored :: Maybe [Text] -> SHA1 -> Appl Bool
commitIgnored Nothing _ = return False
commitIgnored (Just names) (SHA1 commit) = not <$> fold
  (grep (asum (map text names))
    (inproc "git" ["show", "-s", "--format=%B", commit] empty))
  Fold.null

-- |Check if commit/pr is present in changelog. Return '@True@' if present.
changelogIsUp :: Link -> Commit -> FilePath -> Appl Bool
changelogIsUp repoUrl commit@Commit{..} changelog = do
  Options{..} <- asks envOptions
  noEntry <- case commitIsPR of
    Nothing -> fold (grep (has (text (getSHA1 commitSHA))) (input changelog)) Fold.null
    Just (PR num) -> fold (grep (has (text num)) (input changelog)) Fold.null
  if noEntry
    then do
      -- If --from-bc option invoked it will prepend list of misses with version tag.
      printCommitTag commitSHA
      if optSuggest
        then suggestMissing repoUrl commit
        else warnMissing commit
      when (optAction == Just UpdateChangelogs) $ addMissing repoUrl commit changelog
      return False
    else return True
