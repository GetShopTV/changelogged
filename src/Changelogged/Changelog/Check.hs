{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Changelogged.Changelog.Check where

import           Data.Foldable                  (asum)
import           Prelude                        hiding (FilePath)
import           Turtle                         hiding (find, stderr, stdout)

import qualified Control.Foldl                  as Fold

import           Changelogged.Changelog.Compose
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
      flags <- mapM (checkCommit gitInfo config) (map SHA1 commitHashes)
      if and flags
        then success (showPath changelogChangelog <> " is up to date.\n" <> "You can run bump-versions to bump versions for it.")
        else do
          warning $ showPath changelogChangelog <> " is out of date." <> "\nRun changelogged to update it interactively."
      return $ and flags
    else mapM_ (checkCommit gitInfo config) (map SHA1 commitHashes) >> return True

  return upToDate

checkCommit :: GitInfo -> ChangelogConfig -> SHA1 -> Appl Bool
checkCommit GitInfo{..} ChangelogConfig{..} commitSHA = do
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
      then changelogIsUp Commit{..} changelogChangelog
      else interactiveChangelogIsUp gitRemoteUrl Commit{..} changelogChangelog

allFilesIgnored :: Maybe [FilePath] -> SHA1 -> Appl Bool
allFilesIgnored Nothing _ = return False
allFilesIgnored (Just files) (SHA1 commit) = do
  affectedFiles <- fold (inproc "git" ["diff-tree", "--name-only", "--no-commit-id", "-m", "-r", commit] empty) Fold.list
  let expandIgnoredFiles = map makeWildcardPattern (map encodeString files)
  return . null . filter null . map (match (choice expandIgnoredFiles)) $ (map lineToText affectedFiles)

commitNotWatched :: Maybe [FilePath] -> SHA1 -> Appl Bool
commitNotWatched Nothing _ = return False
commitNotWatched (Just files) (SHA1 commit) = let expandWatchFiles = asum (map makeWildcardPattern (map encodeString files)) in
  fold
      (grep expandWatchFiles
      (inproc "git" ["diff-tree", "--name-only", "--no-commit-id", "-m", "-r", commit] empty))
    Fold.null

commitIgnored :: Maybe [Text] -> SHA1 -> Appl Bool
commitIgnored Nothing _ = return False
commitIgnored (Just names) (SHA1 commit) = not <$> fold
  (grep (asum (map text names))
    (inproc "git" ["show", "-s", "--format=%B", commit] empty))
  Fold.null

interactiveSession :: Link -> Commit -> FilePath -> Appl Bool
interactiveSession _repoUrl _commit@Commit{..} _changelog = return True

interactiveChangelogIsUp :: Link -> Commit -> FilePath -> Appl Bool
interactiveChangelogIsUp repoUrl commit@Commit{..} changelog =
  actOnMissingCommit commit changelog (interactiveSession repoUrl commit changelog)

-- |Check if commit/pr is present in changelog. Return '@True@' if present.
changelogIsUp :: Commit -> FilePath -> Appl Bool
changelogIsUp commit@Commit{..} changelog =
  actOnMissingCommit commit changelog $ do
    warnMissing commit
    return False

actOnMissingCommit :: Commit -> FilePath -> Appl Bool -> Appl Bool
actOnMissingCommit Commit{..} changelog action = do
  noEntry <- case commitIsPR of
    Nothing -> fold (grep (has (text (getSHA1 commitSHA))) (input changelog)) Fold.null
    Just (PR num) -> fold (grep (has (text num)) (input changelog)) Fold.null
  if noEntry
    -- If --from-bc option invoked it will prepend list of misses with version tag.
    then printCommitTag commitSHA >> action
    else return True
