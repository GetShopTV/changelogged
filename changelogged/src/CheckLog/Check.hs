{-# LANGUAGE RecordWildCards #-}
module CheckLog.Check where

import Turtle
import Prelude hiding (FilePath, log)

import qualified Data.Text as Text

import Control.Monad (when, unless)

import System.Console.ANSI (Color(..))

import Types
import CheckLog.Common

checkChangelogF :: WarningFormat -> Git -> FilePath -> IO Bool
checkChangelogF fmt Git{..} changelog = do
  printf ("Checking "%fp%"\n") changelog

  stdout $ inproc "egrep" ["-o", "^[0-9a-f]+"] (inproc "egrep" [pullExpr, showPath gitHistory] empty)
  pullCommits <- strict $
    inproc "egrep" ["-o", "^[0-9a-f]+"] (inproc "egrep" [pullExpr] (input gitHistory))
  pulls <- strict $
    inproc "egrep" ["-o", "#[0-9]+"] (inproc "egrep" ["-o", pullExpr] (input gitHistory))
  singles <- strict $
    inproc "egrep" ["-o", "^[0-9a-f]+"] (inproc "egrep" ["-o", singleExpr] (input gitHistory))
  
  pullHeaders <- mapM (commitMessage PR . Text.stripEnd) (Text.split (== '\n') pullCommits)
  singleHeaders <- mapM (commitMessage Commit . Text.stripEnd) (Text.split (== '\n') singles)
  flagsPR <- mapM (\(i,m) -> changelogIsUp fmt gitLink i PR Project m changelog) (zip (Text.split (== '\n') pulls) pullHeaders)
  flagsCommit <- mapM (\(i, m) -> changelogIsUp fmt gitLink i Commit Project m changelog) (zip (Text.split (== '\n') singles) singleHeaders)
  return $ and (flagsPR ++ flagsCommit)
  where
    pullExpr = "pull request #[0-9]+"
    singleExpr = "^[0-9a-f]+\\s[^(Merge)]"

checkApiChangelogF :: WarningFormat -> Git -> FilePath -> FilePath -> IO Bool
checkApiChangelogF fmt Git{..} swaggerFile changelog = do
  printf ("Checking "%fp%"\n") changelog
  
  commits <- strict $ inproc "egrep" ["-o", "^[0-9a-f]+"] (input gitHistory)
  
  flags <- mapM (eval gitHistory) (Text.split (== '\n') (Text.stripEnd commits))
  return $ and flags
  where
    eval hist commit = do
      linePresent <- fold
        (inproc "grep" [showPath swaggerFile]
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

processChecks :: Options -> Bool -> Maybe FilePath -> FilePath -> FilePath -> IO ()
processChecks _ True _ _ _ = coloredPrint Yellow "WARNING: skipping checks for changelog.\n"
processChecks Options{..} False swagger changelog apiChangelog = do
  when optFromBC $ echo "Checking changelogs from start of project"
  git <- gitData optFromBC
  upToDate <- checkChangelogF optFormat git changelog
  if upToDate
    then coloredPrint Green (showPath changelog <> " is up to date.\n")
    else coloredPrint Yellow ("WARNING: " <> showPath changelog <> " is out of date.\n")
  apiUpToDate <- case swagger of
    Nothing -> do
      coloredPrint Yellow "Do not check API changelog, no swagger file added to ./paths.\n"
      return True
    Just file -> checkApiChangelogF optFormat git file apiChangelog
  if apiUpToDate
    then coloredPrint Green (showPath apiChangelog <> " is up to date.\n")
    else coloredPrint Yellow ("WARNING: " <> showPath apiChangelog <> " is out of date.\n")
  sh $ rm $ gitHistory git
  unless (apiUpToDate && upToDate) $ do
      coloredPrint Red "ERROR: some changelogs are not up-to-date. Use -c or --no-check options if you want to ignore changelog checks and -f to bump anyway.\n"
      unless optForce $ exit ExitSuccess
