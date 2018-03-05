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
  config <- fromMaybe defaultConfig <$> loadConfig "changelogged.yaml"
  -- load git info
  gitInfo <- loadGitInfo optFromBC
  coloredPrint Cyan (ppGitInfo gitInfo)
  -- process changelogs
  processChangelogs config opts gitInfo

processChangelogs :: Config -> Options -> GitInfo -> IO ()
processChangelogs config opts gitInfo = do
  mapM_ (processChangelog opts gitInfo) (configChangelogs config)

processChangelog :: Options -> GitInfo -> ChangelogConfig -> IO ()
processChangelog opts@Options{..} gitInfo config@ChangelogConfig{..} = do
  coloredPrint Green ("Checking " <> format fp changelogChangelog <> " and creating it if missing.\n")
  touch changelogChangelog

  bump <- if optNoCheck
    then do
      warning $ "skipping checks for " <> format fp changelogChangelog <> " (due to --no-check).\n"
      return True
    else do
      checkChangelogWrap opts gitInfo config

  (when (bump && optBumpVersions) $ do
    newVersion <- if optNoCheck
      then do
        warning "cannot infer new version from changelog because of --no-check"
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

