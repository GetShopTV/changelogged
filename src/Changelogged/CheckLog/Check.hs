{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Changelogged.CheckLog.Check where

import Control.Monad.Catch
import Control.Exception hiding (catch, throw)

import System.Console.ANSI (Color(..))

import Turtle hiding (stdout, stderr, find)
import Prelude hiding (FilePath)
import Data.Foldable (asum)
import Data.List (find)

import qualified Control.Foldl as Fold

import Changelogged.Bump.Local
import Changelogged.Bump.Common
import Changelogged.Types
import Changelogged.Options
import Changelogged.Utils
import Changelogged.Pure
import Changelogged.Pattern
import Changelogged.CheckLog.Common
import Changelogged.Config
import Changelogged.Git

processChangelogs :: Config -> GitInfo -> Appl ()
processChangelogs config gitInfo = do
  Options{..} <- ask
  case optTargetChangelog of
    Nothing -> case length . configChangelogs $ config of
      0 -> failure "You have empty configuration file" 
      1 -> processChangelog gitInfo $ head . configChangelogs $ config
      _ -> failure "You cannot bump versions generally through all changelogs. Correct form: changelogged --bump-versions --level <level> <changelog>"
    Just changelogPath -> do
      case lookupChangelog changelogPath of
        Just changelog -> processChangelog gitInfo changelog
        Nothing -> failure $ "Given target changelog " <> format fp changelogPath <> " is missed in config or mistyped."
      where
        lookupChangelog path = find (\entry -> changelogChangelog entry == path) (configChangelogs config)

bumpVersions :: Bool -> ChangelogConfig -> Appl ()
bumpVersions upToDate config@ChangelogConfig{..} = do
  Options{..} <- ask
  when optBumpVersions $ if
    | not upToDate && not optForce ->
        failure $ "cannot bump versions because " <> format fp changelogChangelog <> " is out of date.\nUse --no-check to skip changelog checks.\nUse --force to force bump version."
    | not upToDate && optForce ->
        warning $ format fp changelogChangelog <> " is out of date. Bumping versions anyway due to --force."
    | otherwise -> (do
        newVersion <- case (optNoCheck, optChangeLevel) of
          (_, Just lev) -> generateLocalVersion lev config
          (True, Nothing) ->  do
            failure "cannot infer new version from changelog because of --no-check.\nUse explicit --level CHANGE_LEVEL."
            return Nothing
          (False, Nothing) -> generateLocalVersionByChangelog config

        case newVersion of
          Nothing -> return ()
          Just version -> case changelogVersionFiles of
            Just versionFiles -> do
              mapM_ (bumpPart version) versionFiles
              headChangelog version changelogChangelog
            Nothing -> warning "no files specified to bump versions in"
        ) `catch` (\(ex :: PatternMatchFail) -> failure (showText ex))
  

checkChangelog :: GitInfo -> ChangelogConfig -> Appl Bool
checkChangelog gitInfo@GitInfo{..} config@ChangelogConfig{..} = do
  Options{..} <- ask
  upToDate <- if optNoCheck
    then do
      warning $ "skipping checks for " <> format fp changelogChangelog <> " (due to --no-check)."
      return True
    else do
      when optFromBC $ printf ("Checking "%fp%" from start of project\n") changelogChangelog
      info $ "looking for missing entries in " <> format fp changelogChangelog
  
      commits <- map (fromJustCustom "Cannot find commit hash in git log entry" . hashMatch . lineToText)
        <$> fold (select gitHistory) Fold.list
      flags <- mapM (checkCommits gitInfo config) commits
      return $ and flags

  if upToDate
    then coloredPrint Green (showPath changelogChangelog <> " is up to date.\n")
    else do
      warning $ showPath changelogChangelog <> " is out of date." <>
        if optUpdateChangelog
          then ""
          else "\nUse --update-changelog to add missing changelog entries automatically."
  
  return upToDate

processChangelog :: GitInfo -> ChangelogConfig -> Appl ()
processChangelog gitInfo config@ChangelogConfig{..} = do
  Options{..} <- ask
  liftIO $ putStrLn ""
  info $ "processing " <> format fp changelogChangelog
  changelogExists <- testfile changelogChangelog
  when (not changelogExists) $ do
    info (format fp changelogChangelog <> " does not exist. Creating an empty changelog.")
    touch changelogChangelog

  upToDate <- checkChangelog gitInfo config
  bumpVersions upToDate config

checkCommits :: GitInfo -> ChangelogConfig -> Text -> Appl Bool
checkCommits GitInfo{..} ChangelogConfig{..} commit = do
  ignoreChangeReasoned <- sequence $
    [ commitNotWatched changelogWatchFiles commit
    , allFilesIgnored changelogIgnoreFiles commit
    , commitIgnored changelogIgnoreCommits commit]
  if or ignoreChangeReasoned then return True else do
    pull <- fmap (fromJustCustom "Cannot find commit hash in git log entry" . githubRefMatch . lineToText) <$>
        fold (grep githubRefGrep (grep (has (text commit)) (select gitHistory))) Fold.head
    case pull of
      Nothing -> do
        message <- commitMessage Commit commit
        changelogIsUp gitRemoteUrl commit Commit message changelogChangelog
      Just pnum -> do
        message <- commitMessage PR commit
        changelogIsUp gitRemoteUrl pnum PR message changelogChangelog

allFilesIgnored :: Maybe [FilePath] -> Text -> Appl Bool
allFilesIgnored Nothing _ = return False
allFilesIgnored (Just files) commit = do
  affectedFiles <- fold (inproc "git" ["diff-tree", "--name-only", "--no-commit-id", "-m", commit] empty) Fold.list
  return . and $ map (flip elem files . fromText . lineToText) affectedFiles

commitNotWatched :: Maybe [FilePath] -> Text -> Appl Bool
commitNotWatched Nothing _ = return False
commitNotWatched (Just files) commit = fold
  (grep (asum (map (text . showPath) files))
    (inproc "git" ["diff-tree", "--name-only", "--no-commit-id", "-m", commit] empty))
  Fold.null

commitIgnored :: Maybe [Text] -> Text -> Appl Bool
commitIgnored Nothing _ = return False
commitIgnored (Just names) commit = not <$> fold
  (grep (asum (map text names))
    (inproc "git" ["show", "-s", "--format=%B", commit] empty))
  Fold.null
