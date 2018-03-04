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

  bump <- checkChangelogWrap opts git optNoCheck config

  (when (bump && optBumpVersions) $ do
    newVersion <- generateLocalVersionByChangelog optNoCheck config

    case newVersion of
      Nothing -> return ()
      Just version -> case changelogVersionFiles of
        Just versionFiles -> do
          mapM_ (bumpPart version) versionFiles
          headChangelog version changelogChangelog
        Nothing -> coloredPrint Yellow "WARNING: no files to bump version in specified.\n"
    ) `catch` (\(ex :: PatternMatchFail) -> coloredPrint Red (showText ex))

