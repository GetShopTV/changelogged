{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Changelogged.Main where

import Data.List (find)
import Turtle hiding (FilePath, find)

import Control.Monad.Catch
import Control.Exception hiding (catch, throw)
import Data.Maybe (fromMaybe)
import Data.Text (unpack, pack)

import System.Console.ANSI (Color(..))

import Changelogged.CheckLog.Check
import Changelogged.Bump.Local
import Changelogged.Bump.Common
import Changelogged.Git
import Changelogged.Options
import Changelogged.Utils
import Changelogged.Types (Level)
import Changelogged.Pure (showText, showPath, changeloggedVersion)
import Changelogged.Config

defaultMain :: IO ()
defaultMain = do
  -- parse command line options
  opts@Options{..} <- parseOptions

  runInAppl opts $ if optVersion
    then versionP changeloggedVersion
    else do
      debugYaml "parsed options:" opts

      -- load config file (or default config)
      let configPath = fromMaybe ".changelogged.yaml" (unpack . showPath <$> optConfigPath)
      config'@Config{..} <- fromMaybe defaultConfig <$> loadConfig configPath
      -- load git info
      gitInfo <- loadGitInfo configBranch
      if config' == defaultConfig
        then coloredPrint Blue "Using default config.\n"
        else coloredPrint Blue ("Configuration file: " <> pack configPath <> "\n")
      
      -- ignore all changelogs by default.
      let changelogs = map changelogChangelog configChangelogs
          config = config' {configChangelogs = map (\cc -> cc {changelogIgnoreFiles = Just changelogs <> changelogIgnoreFiles cc}) configChangelogs}
      
      coloredPrint Blue (ppConfig  config)
      coloredPrint Blue (ppGitInfo gitInfo)
      -- process changelogs
      processChangelogs config gitInfo

processChangelogs :: Config -> GitInfo -> Appl ()
processChangelogs config gitInfo = do
  Options{..} <- ask
  case optTargetChangelog of
    Nothing -> case length . configChangelogs $ config of
      0 -> failure "You have empty configuration file" 
      1 -> processChangelog gitInfo optChangeLevel $ head . configChangelogs $ config
      _ -> failure "You cannot bump versions generally through all changelogs. Correct form: changelogged --bump-versions --level <level> <changelog>"
    Just changelogPath -> do
      case lookupChangelog changelogPath of
        Just changelog -> processChangelog gitInfo optChangeLevel changelog
        Nothing -> failure $ "Given target changelog " <> format fp changelogPath <> " is missed in config or mistyped."
      where
        lookupChangelog path = find (\entry -> changelogChangelog entry == path) (configChangelogs config)

processChangelog :: GitInfo -> Maybe Level -> ChangelogConfig -> Appl ()
processChangelog gitInfo level config@ChangelogConfig{..} = do
  Options{..} <- ask
  liftIO $ putStrLn ""
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
      when optFromBC $ printf ("Checking "%fp%" from start of project\n") changelogChangelog
      checkLocalChangelogF gitInfo config

  if upToDate
    then coloredPrint Green (showPath changelogChangelog <> " is up to date.\n")
    else do
      warning $ showPath changelogChangelog <> " is out of date." <>
        if optUpdateChangelog
          then ""
          else "\nUse --update-changelog to add missing changelog entries automatically."

  when optBumpVersions $ if
    | not upToDate && not optForce ->
        failure $ "cannot bump versions because " <> format fp changelogChangelog <> " is out of date.\nUse --no-check to skip changelog checks.\nUse --force to force bump version."
    | not upToDate && optForce ->
        warning $ format fp changelogChangelog <> " is out of date. Bumping versions anyway due to --force."
    | otherwise -> (do
        newVersion <- case (optNoCheck, level) of
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
