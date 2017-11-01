module CheckLog.Check where

import Turtle
import Prelude hiding (FilePath, log)

import Data.Text (Text)
import qualified Data.Text as T

import qualified Control.Foldl as Fold

import System.Console.ANSI (Color(..))

import Types
import CheckLog.Common

checkChangelogF :: Bool -> Text -> IO Bool
checkChangelogF start changelog = do
  printf ("Checking "%s%"\n") changelog
  
  history <- gitLatestHistory start
  
  pullCommits <- strict $
    inproc "egrep" ["-o", "^[0-9a-f]+"] (inproc "egrep" [pullExpr, history] empty)
  pulls <- strict $
    inproc "egrep" ["-o", "#[0-9]+"] (inproc "egrep" ["-o", pullExpr, history] empty)
  singles <- strict $
    inproc "egrep" ["-o", "^[0-9a-f]+"] (inproc "egrep" ["-o", singleExpr, history] empty)
  
  pullHeaders <- mapM (commitMessage PR) (map T.stripEnd (T.split (== '\n') pullCommits))
  singleHeaders <- mapM (commitMessage Commit) (map T.stripEnd (T.split (== '\n') singles))
  flagsPR <- mapM (\(i,m) -> changelogIsUp i PR Project m changelog) (zip (T.split (== '\n') pulls) pullHeaders)
  flagsCommit <- mapM (\(i, m) -> changelogIsUp i Commit Project m changelog) (zip (T.split (== '\n') singles) singleHeaders)
  return $ foldr1 (&&) (flagsPR ++ flagsCommit)
  where
    pullExpr = "pull request #[0-9]+"
    singleExpr = "^[0-9a-f]+\\s[^(Merge)]"

checkApiChangelogF :: Bool -> Text -> Text -> IO Bool
checkApiChangelogF start swaggerFile changelog = do
  printf ("Checking "%s%"\n") changelog
  
  history <- gitLatestHistory start

  commits <- strict $ inproc "egrep" ["-o", "^[0-9a-f]+", history] empty
  
  flags <- mapM (eval history) (T.split (== '\n') (T.stripEnd commits))
  return $ foldr1 (&&) flags
  where
    eval hist commit = do
      linePresent <- fold
        (inproc "grep" [swaggerFile]
          (inproc "git" ["show", "--stat", commit] empty))
        Fold.length
      case linePresent of
        0 -> return True
        _ -> do
          pull <- strict $
            inproc "egrep" ["-o", "#[0-9]+"] $
              inproc "egrep" ["-o", "pull request #[0-9]+"] $
                inproc "grep" [commit, hist] empty
          case T.length pull of
            0 -> do
              message <- commitMessage Commit commit
              changelogIsUp commit Commit API message changelog
            _ -> do
              message <- commitMessage PR commit
              changelogIsUp (T.stripEnd pull) PR API message changelog

processChecks :: Bool -> Bool -> Bool -> Maybe Text -> Text -> Text -> IO ()
processChecks True _ _ _ _ _ = coloredPrint Yellow "WARNING: skipping checks for changelog.\n"
processChecks False start force swagger changelog apiChangelog = do
  if start
    then echo "Checking changelogs from start of project"
    else return ()
  upToDate <- checkChangelogF start changelog
  if upToDate
    then coloredPrint Green (changelog <> " is up to date.\n")
    else coloredPrint Yellow ("WARNING: " <> changelog <> " is out of date.\n")
  apiUpToDate <- case swagger of
    Nothing -> do
      coloredPrint Yellow "Do not check API changelog, no swagger file added to ./paths.\n"
      return True
    Just file -> checkApiChangelogF start file apiChangelog
  if apiUpToDate
    then coloredPrint Green $ (apiChangelog<>" is up to date.\n")
    else coloredPrint Yellow ("WARNING: " <> apiChangelog <> " is out of date.\n")
  if not (apiUpToDate && upToDate)
    then do
      coloredPrint Red "ERROR: some changelogs are not up-to-date. Use -c or --no-check options if you want to ignore changelog checks and -f to bump anyway.\n"
      if not force
        then exit ExitSuccess
        else return ()
    else return ()
