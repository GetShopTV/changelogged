{-# LANGUAGE ScopedTypeVariables #-}
module Changelogged.Main where

import Turtle hiding (FilePath)

import Control.Exception
import Data.Maybe (fromMaybe)

import System.Console.ANSI (Color(..))

import Changelogged.CheckLog.Check
import Changelogged.Bump.Local
import Changelogged.Bump.Common
import Changelogged.Types
import Changelogged.Git
import Changelogged.Options
import Changelogged.Utils
import Changelogged.Pure (showText)
import Changelogged.Config

defaultMain :: IO ()
defaultMain = do
  -- parse command line options
  opts@Options{..} <- options welcome parser
  -- load config file (or default config)
  config <- fromMaybe defaultConfig <$> loadConfig "changelogged.yaml"
  -- load git info
  git <- gitData optFromBC
  -- process changelogs
  processChangelogs config opts git
  -- ???
  sh $ rm $ gitHistory git

processChangelogs :: Config -> Options -> Git -> IO ()
processChangelogs config opts git = do
  mapM_ (processChangelog opts git) (configChangelogs config)

processChangelog :: Options -> Git -> ChangelogConfig -> IO ()
processChangelog opts@Options{..} git config@ChangelogConfig{..} = do
  coloredPrint Green ("Checking " <> format fp changelogChangelog <> " and creating it if missing.\n")
  touch changelogChangelog

  bump <- if optNoCheck
    then do
      warning $ "skipping checks for " <> format fp changelogChangelog <> " (due to --no-check).\n"
      return True
    else do
      checkChangelogWrap opts git config

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

