{-# LANGUAGE ScopedTypeVariables #-}
module CheckLog.Check where

import Turtle
import Prelude hiding (FilePath)

import qualified Control.Foldl as Fold
import Control.Monad (when, filterM)

import System.Console.ANSI (Color(..))

import Types
import Options
import Utils
import Pure
import Pattern
import CheckLog.Common

-- |This is actually part if '@Main@'
-- Check common changelog.
checkCommonChangelogF :: WarningFormat -> Bool -> Git -> FilePath -> IO Bool
checkCommonChangelogF fmt writeLog Git{..} changelog = do
  printf ("Checking "%fp%"\n") changelog

  pullCommits <- map (fromJustCustom "Cannot find commit hash in git log entry" . hashMatch . lineToText)
    <$> fold (grep githubRefGrep (input gitHistory)) Fold.list
  pulls <- map (fromJustCustom "Cannot find pull request number in git log entry" . githubRefMatch . lineToText)
    <$> fold (grep githubRefGrep (input gitHistory)) Fold.list
  singles <- map (fromJustCustom "Cannot find commit hash in git log entry" . hashMatch . lineToText)
    <$> fold (grep hashGrepExclude (input gitHistory)) Fold.list
  
  filteredSingles <- filterM noMarkdown singles
  
  pullHeaders <- mapM (commitMessage PR) pullCommits
  singleHeaders <- mapM (commitMessage Commit) filteredSingles
  flagsPR <- mapM (\(i,m) -> changelogIsUp fmt writeLog gitLink i PR m changelog) (zip pulls pullHeaders)
  flagsCommit <- mapM (\(i, m) -> changelogIsUp fmt writeLog gitLink i Commit m changelog) (zip filteredSingles singleHeaders)
  return $ and (flagsPR ++ flagsCommit)

-- |This is actually part if '@Main@'
-- Check local changelog - local means what changelog is specific and has some indicator file. If file is changed changelog must change.
checkLocalChangelogF :: WarningFormat -> Bool -> Git -> FilePath -> FilePath -> IO Bool
checkLocalChangelogF fmt writeLog Git{..} path indicator = do
  printf ("Checking "%fp%"\n") path
  
  commits <- map (fromJustCustom "Cannot find commit hash in git log entry" . hashMatch . lineToText)
    <$> fold (input gitHistory) Fold.list

  flags <- mapM (eval gitHistory) commits
  return $ and flags
  where
    eval hist commit = do
      linePresent <- fold
        (grep (has $ text $ showPath indicator)
          (inproc "git" ["show", "--stat", commit] empty))
        countLines
      case linePresent of
        0 -> return True
        _ -> do
          pull <- fmap (fromJustCustom "Cannot find commit hash in git log entry" . githubRefMatch . lineToText) <$>
              fold (grep githubRefGrep (grep (has (text commit)) (input hist))) Fold.head
          case pull of
            Nothing -> do
              message <- commitMessage Commit commit
              changelogIsUp fmt writeLog gitLink commit Commit message path
            Just pnum -> do
              message <- commitMessage PR commit
              changelogIsUp fmt writeLog gitLink pnum PR message path

-- |This is actually part if '@Main@'
-- Check given changelog regarding options.
checkChangelogWrap :: Options -> Git -> Bool -> TaggedLog -> IO Bool
checkChangelogWrap _ _ True _ = do
  coloredPrint Yellow "WARNING: skipping checks for API changelog.\n"
  return True
checkChangelogWrap Options{..} git False TaggedLog{..} = do
  when optFromBC $ printf ("Checking "%fp%" from start of project\n") taggedLogPath
  upToDate <- case taggedLogIndicator of
    Nothing -> checkCommonChangelogF optFormat optWrite git taggedLogPath
    Just ind -> checkLocalChangelogF optFormat optWrite git taggedLogPath (taggedFilePath ind)
  if upToDate
    then coloredPrint Green (showPath taggedLogPath <> " is up to date.\n")
    else coloredPrint Yellow ("WARNING: " <> showPath taggedLogPath <> " is out of date.\n")
  if upToDate
    then return True
    else do
      coloredPrint Red ("ERROR: " <> showPath taggedLogPath <> " is not up-to-date. Use -c or --no-check options if you want to ignore changelog checks and -f to bump anyway.\n")
      return optForce
