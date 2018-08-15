{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Changelogged.Changelog.Check where

import           Prelude                        hiding (FilePath)
import           Turtle                         hiding (find, stderr, stdout)

import           System.Console.ANSI            (Color (..))

import qualified Control.Foldl                  as Fold
import           Control.Monad                  (when)

import           Changelogged.Changelog.Common
import           Changelogged.Changelog.Interactive
import           Changelogged.Changelog.Plain
import           Changelogged.Common
import           Changelogged.Pattern
import           Changelogged.Git (retrieveCommitMessage)

checkChangelog :: GitInfo -> ChangelogConfig -> Appl ()
checkChangelog gitInfo@GitInfo{..} config@ChangelogConfig{..} = do
  Options{..} <- gets envOptions
  when optFromBeginning $ printf ("Checking "%fp%" from start of the project\n") changelogChangelog
  case optFromVersion of
    Nothing -> printf ("Checking "%fp%" from latest version\n") changelogChangelog
    Just tag -> printf ("Checking "%fp%" from "%s%"\n") changelogChangelog tag
  info $ "looking for missing entries in " <> format fp changelogChangelog <> "\n"

  commitHashes <- map (fromJustCustom "Cannot find commit hash in git log entry" . hashMatch . lineToText)
    <$> fold (select gitHistory) Fold.list

  flags <- do 
    upToDate <- mapM (dealWithCommit True False gitInfo config) (map SHA1 commitHashes)
    if optListMisses
      then return upToDate
      else do
        interactiveMode <- promptGoInteractive
        mapM (dealWithCommit False interactiveMode gitInfo config) (map SHA1 commitHashes)
  if and flags
    then success $ showPath changelogChangelog <> " is up to date.\n"
                   <> "You can edit it manually now and arrange levels of changes if not yet.\n"
    else warning $ showPath changelogChangelog <> " does not mention all git history entries.\n"
                   <> "You can run changelogged to update it interactively and bump versions.\n"

promptGoInteractive :: Appl Bool
promptGoInteractive = do
  coloredPrint Yellow $ "You can go to interactive mode or simply write changes to changelog. Go to interactive mode?\n"
  go
  where go = do
          coloredPrint Cyan "(y/n):  \n"
          answer <- liftIO getLine
          case answer of
            "y" -> return True
            "n" -> return False
            _ -> do
              liftIO $ putStrLn "Cannot parse answer. Please repeat."
              go

dealWithCommit :: Bool -> Bool -> GitInfo -> ChangelogConfig -> SHA1 -> Appl Bool
dealWithCommit listMisses interactiveMode GitInfo{..} ChangelogConfig{..} commitSHA = do
  ignoreChangeReasoned <- sequence $
    [ commitNotWatched changelogWatchFiles commitSHA
    , allFilesIgnored changelogIgnoreFiles commitSHA
    , commitIgnored changelogIgnoreCommits commitSHA]
  if or ignoreChangeReasoned then return True else do
    commitIsPR <- fmap (PR . fromJustCustom "Cannot find commit hash in git log entry" . githubRefMatch . lineToText) <$>
        fold (grep githubRefGrep (grep (has (text (getSHA1 commitSHA))) (select gitHistory))) Fold.head
    commitMessage <- retrieveCommitMessage commitIsPR commitSHA
    if listMisses
      then plainDealWithEntry Commit{..} changelogChangelog
      else do
        if interactiveMode
          then interactiveDealWithEntry gitRemoteUrl Commit{..} changelogChangelog 
          else simpleDealWithEntry gitRemoteUrl Commit{..} changelogChangelog
