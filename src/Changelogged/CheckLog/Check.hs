{-# LANGUAGE ScopedTypeVariables #-}
module Changelogged.CheckLog.Check where

import Turtle hiding (stdout, stderr)
import Prelude hiding (FilePath)
import Data.Foldable (asum)

import qualified Control.Foldl as Fold
import Control.Monad (when)

import System.Console.ANSI (Color(..))

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
checkLocalChangelogF :: WarningFormat -> Bool -> GitInfo -> ChangelogConfig -> Appl Bool
checkLocalChangelogF fmt writeLog GitInfo{..} ChangelogConfig{..} = do
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
            changelogIsUp fmt writeLog gitRemoteUrl commit Commit message changelogChangelog
          Just pnum -> do
            message <- commitMessage PR commit
            changelogIsUp fmt writeLog gitRemoteUrl pnum PR message changelogChangelog

-- |This is actually part of '@Main@'
-- Check given changelog regarding options.
checkChangelogWrap :: GitInfo -> ChangelogConfig -> Appl Bool
checkChangelogWrap git config@ChangelogConfig{..} = do
  Options{..} <- ask
  if (optUpdateChangelog && optFormat == WarnSimple)
    then do
      failure "--update-changelog does not work with --format=simple (try --format=suggest instead)"
      return False
    else do
      when optFromBC $ printf ("Checking "%fp%" from start of project\n") changelogChangelog
      upToDate <- checkLocalChangelogF optFormat optUpdateChangelog git config
      if upToDate
        then coloredPrint Green (showPath changelogChangelog <> " is up to date.\n")
        else do
          warning $ showPath changelogChangelog <> " is out of date." <>
            if optUpdateChangelog then "" else
              "\nUse --update-changelog to add missing changelog entries automatically."
      return upToDate
