{-# LANGUAGE ScopedTypeVariables #-}
module Changelogged.CheckLog.Check where

import Turtle hiding (stdout, stderr)
import Prelude hiding (FilePath)
import Data.Foldable (asum)

import qualified Control.Foldl as Fold
import Control.Monad (when, filterM)

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
-- Check common changelog.
checkCommonChangelogF :: WarningFormat -> Bool -> GitInfo -> FilePath -> IO Bool
checkCommonChangelogF fmt writeLog GitInfo{..} changelog = do
  printf ("Checking "%fp%"\n") changelog

  pullCommits <- map (fromJustCustom "Cannot find commit hash in git log entry" . hashMatch . lineToText)
    <$> fold (grep githubRefGrep (select gitHistory)) Fold.list
  pulls <- map (fromJustCustom "Cannot find pull request number in git log entry" . githubRefMatch . lineToText)
    <$> fold (grep githubRefGrep (select gitHistory)) Fold.list
  singles <- map (fromJustCustom "Cannot find commit hash in git log entry" . hashMatch . lineToText)
    <$> fold (grep hashGrepExclude (select gitHistory)) Fold.list
  
  filteredSingles <- filterM noMarkdown singles
  
  pullHeaders <- mapM (commitMessage PR) pullCommits
  singleHeaders <- mapM (commitMessage Commit) filteredSingles
  flagsPR <- mapM (\(i,m) -> changelogIsUp fmt writeLog gitRemoteUrl i PR m changelog) (zip pulls pullHeaders)
  flagsCommit <- mapM (\(i, m) -> changelogIsUp fmt writeLog gitRemoteUrl i Commit m changelog) (zip filteredSingles singleHeaders)
  return $ and (flagsPR ++ flagsCommit)

-- |This is actually part if '@Main@'
-- Check local changelog - local means what changelog is specific and has some indicator file. If file is changed changelog must change.
checkLocalChangelogF :: WarningFormat -> Bool -> GitInfo -> FilePath -> [FilePath] -> IO Bool
checkLocalChangelogF fmt writeLog GitInfo{..} path versionFilePaths = do
  printf ("Checking "%fp%"\n") path
  
  commits <- map (fromJustCustom "Cannot find commit hash in git log entry" . hashMatch . lineToText)
    <$> fold (select gitHistory) Fold.list

  flags <- mapM eval commits
  return $ and flags
  where
    eval commit = do
      linePresent <- fold
        (grep (asum (map (has . text . showPath) versionFilePaths))
          (inproc "git" ["show", "--stat", commit] empty))
        countLines
      case linePresent of
        0 -> return True
        _ -> do
          pull <- fmap (fromJustCustom "Cannot find commit hash in git log entry" . githubRefMatch . lineToText) <$>
              fold (grep githubRefGrep (grep (has (text commit)) (select gitHistory))) Fold.head
          case pull of
            Nothing -> do
              message <- commitMessage Commit commit
              changelogIsUp fmt writeLog gitRemoteUrl commit Commit message path
            Just pnum -> do
              message <- commitMessage PR commit
              changelogIsUp fmt writeLog gitRemoteUrl pnum PR message path

-- |This is actually part if '@Main@'
-- Check given changelog regarding options.
checkChangelogWrap :: Options -> GitInfo -> ChangelogConfig -> IO Bool
checkChangelogWrap Options{..} git ChangelogConfig{..} = do
  if (optUpdateChangelog && optFormat == WarnSimple)
    then do
      failure "--update-changelog does not work with --format=simple (try --format=suggest instead)"
      return False
    else do
      when optFromBC $ printf ("Checking "%fp%" from start of project\n") changelogChangelog
      upToDate <- case changelogVersionFiles of
        Nothing -> checkCommonChangelogF optFormat optUpdateChangelog git changelogChangelog
        Just versionFiles -> checkLocalChangelogF optFormat optUpdateChangelog git changelogChangelog (map versionFilePath versionFiles)
      if upToDate
        then coloredPrint Green (showPath changelogChangelog <> " is up to date.\n")
        else warning $ showPath changelogChangelog <> " is out of date"
      if upToDate
        then return True
        else do
          failure $ showPath changelogChangelog <> " is out of date. Use --no-check if you want to ignore changelog checks and --force to force bump version."
          return optForce
