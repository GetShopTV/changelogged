{-# LANGUAGE ScopedTypeVariables #-}
module Changelogged.EntryPoint where

import System.Console.ANSI (Color(..))

import Turtle hiding (find)
import Prelude hiding (FilePath)
import Data.List (find)

import Changelogged.Versions.Bump
import Changelogged.Changelog.Check
import Changelogged.Options
import Changelogged.Types
import Changelogged.Pure (showPath)
import Changelogged.Utils
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
  case optAction of
    Just BumpVersions -> bumpVersions upToDate config
    Just UpdateChangelogs -> coloredPrint Green (showPath changelogChangelog <> " is updated.\n" <> "You can edit it manually or run bump-versions.\n")
    Nothing -> return ()
