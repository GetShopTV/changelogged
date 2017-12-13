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

-- |Ignore commits which only affect '.md' files
noMarkdown :: Text -> IO Bool
noMarkdown commit = do
  statCommit <- fold (inproc "git" ["show", "--stat", commit] empty) Fold.list
  chLogUpdated <- fold
    (grep (has $ text ".md ")
      (select statCommit)) countLines
  onlyChLogUpdated <- fold
    (grep (has $ text "|")
      (select statCommit)) countLines
  return $ chLogUpdated /= onlyChLogUpdated

checkCommonChangelogF :: WarningFormat -> Git -> FilePath -> IO Bool
checkCommonChangelogF fmt Git{..} changelog = do
  printf ("Checking "%fp%"\n") changelog

  pullCommits <- fmap lineToText <$> fold
    (inproc "egrep" ["-o", "^[0-9a-f]+"] (inproc "egrep" [pullExpr] (input gitHistory))) Fold.list
  pulls <- fmap lineToText <$> fold
    (inproc "egrep" ["-o", "#[0-9]+"] (inproc "egrep" ["-o", pullExpr] (input gitHistory))) Fold.list
  singles <- fmap lineToText <$> fold
    (inproc "egrep" ["-o", "^[0-9a-f]+"] (inproc "grep" ["-o", "-P", singleExpr] (input gitHistory))) Fold.list
  
  filteredSingles <- filterM noMarkdown singles
  
  pullHeaders <- mapM (commitMessage PR) pullCommits
  singleHeaders <- mapM (commitMessage Commit) filteredSingles
  flagsPR <- mapM (\(i,m) -> changelogIsUp fmt gitLink i PR m changelog) (zip pulls pullHeaders)
  flagsCommit <- mapM (\(i, m) -> changelogIsUp fmt gitLink i Commit m changelog) (zip filteredSingles singleHeaders)
  return $ and (flagsPR ++ flagsCommit)
  where
    pullExpr = "pull request #[0-9]+"
    singleExpr = "^[0-9a-f]+\\s(?!.*Merge)"

checkLocalChangelogF :: WarningFormat -> Git -> FilePath -> FilePath -> IO Bool
checkLocalChangelogF fmt Git{..} path indicator = do
  printf ("Checking "%fp%"\n") path
  
  commits <- fmap lineToText <$> fold (inproc "egrep" ["-o", "^[0-9a-f]+"] (input gitHistory)) Fold.list
  
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
          pull <- strict $
            inproc "egrep" ["-o", "#[0-9]+"] $
              inproc "egrep" ["-o", "pull request #[0-9]+"] $
                grep (has (text commit)) (input hist)
          case Text.length pull of
            0 -> do
              message <- commitMessage Commit commit
              changelogIsUp fmt gitLink commit Commit message path
            _ -> do
              message <- commitMessage PR commit
              changelogIsUp fmt gitLink (Text.stripEnd pull) PR message path

checkChangelogWrap :: Options -> Git -> Bool -> TaggedLog -> IO Bool
checkChangelogWrap _ _ True _ = do
  coloredPrint Yellow "WARNING: skipping checks for API changelog.\n"
  return True
checkChangelogWrap Options{..} git False TaggedLog{..} = do
  when optFromBC $ printf ("Checking "%fp%" from start of project") taggedLogPath
  upToDate <- case taggedLogIndicator of
    Nothing -> checkCommonChangelogF optFormat git taggedLogPath
    Just ind -> checkLocalChangelogF optFormat git taggedLogPath (taggedFilePath ind)
  if upToDate
    then coloredPrint Green (showPath taggedLogPath <> " is up to date.\n")
    else coloredPrint Yellow ("WARNING: " <> showPath taggedLogPath <> " is out of date.\n")
  if upToDate
    then return True
    else do
      coloredPrint Red ("ERROR: " <> showPath taggedLogPath <> " is not up-to-date. Use -c or --no-check options if you want to ignore changelog checks and -f to bump anyway.\n")
      return $ if optForce
        then True
        else False
