{-# LANGUAGE RecordWildCards #-}
module CheckLog.Check where

import Turtle
import Prelude hiding (FilePath, log)

import Data.Text (Text)
import qualified Data.Text as Text

import Control.Monad (when, unless)
import qualified Control.Foldl as Fold

import Filesystem.Path.CurrentOS (encodeString)
import System.Console.ANSI (Color(..))

import Types
import CheckLog.Common

checkChangelogF :: WarningFormat -> Git -> Text -> IO Bool
checkChangelogF fmt Git{..} changelog = do
  printf ("Checking "%s%"\n") changelog
  
  pullCommits <- strict $
    inproc "egrep" ["-o", "^[0-9a-f]+"] (inproc "egrep" [pullExpr, historyF] empty)
  pulls <- strict $
    inproc "egrep" ["-o", "#[0-9]+"] (inproc "egrep" ["-o", pullExpr, historyF] empty)
  singles <- strict $
    inproc "egrep" ["-o", "^[0-9a-f]+"] (inproc "egrep" ["-o", singleExpr, historyF] empty)
  
  pullHeaders <- mapM (commitMessage PR . Text.stripEnd) (Text.split (== '\n') pullCommits)
  singleHeaders <- mapM (commitMessage Commit . Text.stripEnd) (Text.split (== '\n') singles)
  flagsPR <- mapM (\(i,m) -> changelogIsUp fmt gitLink i PR Project m changelog) (zip (Text.split (== '\n') pulls) pullHeaders)
  flagsCommit <- mapM (\(i, m) -> changelogIsUp fmt gitLink i Commit Project m changelog) (zip (Text.split (== '\n') singles) singleHeaders)
  return $ and (flagsPR ++ flagsCommit)
  where
    historyF = Text.pack . encodeString $ gitHistory
    pullExpr = "pull request #[0-9]+"
    singleExpr = "^[0-9a-f]+\\s[^(Merge)]"

checkApiChangelogF :: WarningFormat -> Git -> Text -> Text -> IO Bool
checkApiChangelogF fmt Git{..} swaggerFile changelog = do
  printf ("Checking "%s%"\n") changelog
  
  commits <- strict $ inproc "egrep" ["-o", "^[0-9a-f]+", historyF] empty
  
  flags <- mapM (eval historyF) (Text.split (== '\n') (Text.stripEnd commits))
  return $ and flags
  where
    historyF = Text.pack . encodeString $ gitHistory
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
          case Text.length pull of
            0 -> do
              message <- commitMessage Commit commit
              changelogIsUp fmt gitLink commit Commit API message changelog
            _ -> do
              message <- commitMessage PR commit
              changelogIsUp fmt gitLink (Text.stripEnd pull) PR API message changelog

processChecks :: WarningFormat -> Bool -> Bool -> Bool -> Maybe Text -> Text -> Text -> IO ()
processChecks _ True _ _ _ _ _ = coloredPrint Yellow "WARNING: skipping checks for changelog.\n"
processChecks fmt False start force swagger changelog apiChangelog = do
  when start $ echo "Checking changelogs from start of project"
  git <- gitData start
  upToDate <- checkChangelogF fmt git changelog
  if upToDate
    then coloredPrint Green (changelog <> " is up to date.\n")
    else coloredPrint Yellow ("WARNING: " <> changelog <> " is out of date.\n")
  apiUpToDate <- case swagger of
    Nothing -> do
      coloredPrint Yellow "Do not check API changelog, no swagger file added to ./paths.\n"
      return True
    Just file -> checkApiChangelogF fmt git file apiChangelog
  if apiUpToDate
    then coloredPrint Green (apiChangelog<>" is up to date.\n")
    else coloredPrint Yellow ("WARNING: " <> apiChangelog <> " is out of date.\n")
  unless (apiUpToDate && upToDate) $ do
      coloredPrint Red "ERROR: some changelogs are not up-to-date. Use -c or --no-check options if you want to ignore changelog checks and -f to bump anyway.\n"
      unless force $ exit ExitSuccess
