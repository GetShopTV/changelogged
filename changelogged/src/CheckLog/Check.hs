{-# LANGUAGE RecordWildCards #-}
module CheckLog.Check where

import Turtle
import Prelude hiding (FilePath, log)

import qualified Data.Text as Text

import qualified Control.Foldl as Fold
import Control.Monad (when, filterM)

import System.Console.ANSI (Color(..))

import Types
import Utils
import Pure
import CheckLog.Common

notOnlyUpdate :: FilePath -> Text -> IO Bool
notOnlyUpdate changelog commit = do
  statCommit <- fold (inproc "git" ["show", "--stat", commit] empty) Fold.list
  chLogUpdated <- fold
    (grep (has $ text $showPath changelog)
      (select statCommit)) countLines
  onlyChLogUpdated <- fold
    (grep (has $ text "|")
      (select statCommit)) countLines
  return $ chLogUpdated /= onlyChLogUpdated

checkChangelogF :: WarningFormat -> Git -> FilePath -> IO Bool
checkChangelogF fmt Git{..} changelog = do
  printf ("Checking "%fp%"\n") changelog

  pullCommits <- fmap lineToText <$> fold
    (inproc "egrep" ["-o", "^[0-9a-f]+"] (inproc "egrep" [pullExpr] (input gitHistory))) Fold.list
  pulls <- fmap lineToText <$> fold
    (inproc "egrep" ["-o", "#[0-9]+"] (inproc "egrep" ["-o", pullExpr] (input gitHistory))) Fold.list
  singles <- fmap lineToText <$> fold
    (inproc "egrep" ["-o", "^[0-9a-f]+"] (inproc "grep" ["-o", "-P", singleExpr] (input gitHistory))) Fold.list
  
  filteredSingles <- filterM (notOnlyUpdate changelog) singles
  
  pullHeaders <- mapM (commitMessage PR) pullCommits
  singleHeaders <- mapM (commitMessage Commit) filteredSingles
  flagsPR <- mapM (\(i,m) -> changelogIsUp fmt gitLink i PR Project m changelog) (zip pulls pullHeaders)
  flagsCommit <- mapM (\(i, m) -> changelogIsUp fmt gitLink i Commit Project m changelog) (zip filteredSingles singleHeaders)
  return $ and (flagsPR ++ flagsCommit)
  where
    pullExpr = "pull request #[0-9]+"
    singleExpr = "^[0-9a-f]+\\s(?!.*Merge)"

checkApiChangelogF :: WarningFormat -> Git -> FilePath -> FilePath -> IO Bool
checkApiChangelogF fmt Git{..} swaggerFile changelog = do
  printf ("Checking "%fp%"\n") changelog
  
  commits <- fmap lineToText <$> fold (inproc "egrep" ["-o", "^[0-9a-f]+"] (input gitHistory)) Fold.list
  
  flags <- mapM (eval gitHistory) commits
  return $ and flags
  where
    eval hist commit = do
      linePresent <- fold
        (grep (has $ text $ showPath swaggerFile)
          (inproc "git" ["show", "--stat", commit] empty))
        countLines
      case linePresent of
        0 -> return True
        _ -> do
          pull <- strict $
            inproc "egrep" ["-o", "#[0-9]+"] $
              inproc "egrep" ["-o", "pull request #[0-9]+"] $
                grep (has (text commit)) (input hist)
          case Text.length pull of
            0 -> do
              message <- commitMessage Commit commit
              changelogIsUp fmt gitLink commit Commit API message changelog
            _ -> do
              message <- commitMessage PR commit
              changelogIsUp fmt gitLink (Text.stripEnd pull) PR API message changelog

checkChangelogWrap :: Options -> Git -> Bool -> FilePath -> IO Bool
checkChangelogWrap _ _ True _ = do
  coloredPrint Yellow "WARNING: skipping checks for changelog.\n"
  return True
checkChangelogWrap Options{..} git False changelog = do
  when optFromBC $ echo "Checking project changelog from start of project"
  upToDate <- checkChangelogF optFormat git changelog
  if upToDate
    then coloredPrint Green (showPath changelog <> " is up to date.\n")
    else coloredPrint Yellow ("WARNING: " <> showPath changelog <> " is out of date.\n")
  if upToDate
    then return True
    else do
      coloredPrint Red "ERROR: project changelog is not up-to-date. Use -c or --no-check options if you want to ignore changelog checks and -f to bump anyway.\n"
      return $ if optForce
        then True
        else False

checkAPIChangelogWrap :: Options -> Git -> Bool -> Maybe FilePath -> FilePath -> IO Bool
checkAPIChangelogWrap _ _ True _ _ = do
  coloredPrint Yellow "WARNING: skipping checks for API changelog.\n"
  return True
checkAPIChangelogWrap Options{..} git False swagger apiChangelog = do
  when optFromBC $ echo "Checking API changelog from start of project"
  apiUpToDate <- case swagger of
    Nothing -> do
      coloredPrint Yellow "Do not check API changelog, no swagger file added to ./paths.\n"
      return True
    Just file -> checkApiChangelogF optFormat git file apiChangelog
  if apiUpToDate
    then coloredPrint Green (showPath apiChangelog <> " is up to date.\n")
    else coloredPrint Yellow ("WARNING: " <> showPath apiChangelog <> " is out of date.\n")
  if apiUpToDate
    then return True
    else do
      coloredPrint Red "ERROR: API changelog is not up-to-date. Use -c or --no-check options if you want to ignore changelog checks and -f to bump anyway.\n"
      return $ if optForce
        then True
        else False
