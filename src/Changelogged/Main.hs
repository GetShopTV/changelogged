{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Changelogged.Main where

import Turtle hiding (FilePath)

import Control.Exception
import Data.Maybe (fromMaybe)

import System.Console.ANSI (Color(..))

import Changelogged.CheckLog.Check
import Changelogged.Bump.Local
import Changelogged.Bump.Common
import Changelogged.Git
import Changelogged.Options
import Changelogged.Utils
import Changelogged.Pure (showText)
import Changelogged.Config

defaultMain :: IO ()
defaultMain = do
  -- parse command line options
  opts@Options{..} <- parseOptions
  -- load config file (or default config)
  config@Config{..} <- fromMaybe defaultConfig <$> loadConfig "changelogged.yaml"
  -- load git info
  gitInfo <- loadGitInfo optFromBC configBranch
  coloredPrint Blue (ppGitInfo gitInfo)
  -- process changelogs
  processChangelogs config opts gitInfo

processChangelogs :: Config -> Options -> GitInfo -> IO ()
processChangelogs config opts gitInfo = do
  mapM_ (processChangelog opts gitInfo) (configChangelogs config)

processChangelog :: Options -> GitInfo -> ChangelogConfig -> IO ()
processChangelog opts@Options{..} gitInfo config@ChangelogConfig{..} = do
  info $ "processing " <> format fp changelogChangelog
  changelogExists <- testfile changelogChangelog
  when (not changelogExists) $ do
    info (format fp changelogChangelog <> " does not exist. Creating an empty changelog.")
    touch changelogChangelog

  upToDate <- if optNoCheck
    then do
      warning $ "skipping checks for " <> format fp changelogChangelog <> " (due to --no-check)."
      return True
    else do
      checkChangelogWrap opts gitInfo config

  when optBumpVersions $ if
    | not upToDate && not optForce ->
        failure $ "cannot bump versions because " <> format fp changelogChangelog <> " is out of date.\nUse --no-check to skip changelog checks.\nUse --force to force bump version."
    | not upToDate && optForce ->
        warning $ format fp changelogChangelog <> " is out of date. Bumping versions anyway due to --force."
    | otherwise -> (do
        newVersion <- if optNoCheck
          then do
            failure "cannot infer new version from changelog because of --no-check.\nUse explicit --level CHANGE_LEVEL."
            return Nothing
          else do
            generateLocalVersionByChangelog config

        case newVersion of
          Nothing -> return ()
          Just version -> case changelogVersionFiles of
            Just versionFiles -> do
              mapM_ (bumpPart version) versionFiles
              headChangelog version changelogChangelog
            Nothing -> warning "no files specified to bump versions in"
        ) `catch` (\(ex :: PatternMatchFail) -> failure (showText ex))

