{-# LANGUAGE ScopedTypeVariables #-}
module Changelogged.CheckLog.Check where

import Turtle hiding (stdout, stderr)
import Prelude hiding (FilePath)
import Data.Foldable (asum)

import qualified Control.Foldl as Fold

import Changelogged.Types
import Changelogged.Options
import Changelogged.Utils
import Changelogged.Pure
import Changelogged.Pattern
import Changelogged.CheckLog.Common
import Changelogged.Config
import Changelogged.Git

-- |This is actually part if '@Main@'
-- Check local changelog - local means what changelog is specific and has some indicator file. If file is changed changelog must change.
checkLocalChangelogF :: GitInfo -> ChangelogConfig -> Appl Bool
checkLocalChangelogF GitInfo{..} ChangelogConfig{..} = do
  info $ "looking for missing entries in " <> format fp changelogChangelog
  
  commits <- map (fromJustCustom "Cannot find commit hash in git log entry" . hashMatch . lineToText)
    <$> fold (select gitHistory) Fold.list

  flags <- mapM eval commits
  return $ and flags
  where
    eval commit = do
      ignoreChange <- case changelogWatchFiles of
        Nothing -> case changelogIgnoreFiles of
          Nothing -> return False
          Just files -> do
            linePresent <- fold
              (grep (invert (asum (map (has . text . showPath) files)))
                (inproc "git" ["show", "--stat", commit] empty))
              countLines
            return (linePresent == 0)
        Just files -> do
          linePresent <- fold
            (grep (asum (map (has . text . showPath) files))
              (inproc "git" ["show", "--stat", commit] empty))
            countLines
          return (linePresent == 0)
      if ignoreChange then return True else do
        pull <- fmap (fromJustCustom "Cannot find commit hash in git log entry" . githubRefMatch . lineToText) <$>
            fold (grep githubRefGrep (grep (has (text commit)) (select gitHistory))) Fold.head
        case pull of
          Nothing -> do
            message <- commitMessage Commit commit
            changelogIsUp gitRemoteUrl commit Commit message changelogChangelog
          Just pnum -> do
            message <- commitMessage PR commit
            changelogIsUp gitRemoteUrl pnum PR message changelogChangelog
