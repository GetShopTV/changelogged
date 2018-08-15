{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import           Turtle                  hiding (FilePath)

import           Data.Maybe              (fromMaybe)
import           Data.Text               (pack, unpack)

import           System.Console.ANSI     (Color (..))

import           Changelogged.EntryPoint

import           Changelogged.Common
import           Changelogged.Config
import           Changelogged.Options

prepareConfig :: FilePath -> Options -> IO Config
prepareConfig configPath Options{..} = do
  configExists <- testfile (decodeString configPath)
  -- load config file (or default config)
  if configExists
    then do
    coloredPrintIO (optNoColors optionsCommon) Magenta ("Using config file at "<> pack configPath <>"\n")
    eitherConf <- loadConfig configPath
    case eitherConf of
      Left parseError -> (die . showText) parseError
      Right conf -> return conf
    else do
      coloredPrintIO (optNoColors optionsCommon) Magenta ("Config file at "<> pack configPath <>" not found. Using default config.\n")
      return defaultConfig

main :: IO ()
main = do
  -- parse command line options
  envOptions@Options{..} <- parseOptions

  let configPath = fromMaybe ".changelogged.yaml" (unpack . showPath <$> optConfigPath optionsCommon)
  envConfig@Config{..} <- prepareConfig configPath envOptions

  runInAppl ChangeloggedEnv{..} $ if optVersion optionsCommon
    then versionP changeloggedVersion
    else do
      debugYaml "parsed options:" envOptions
      debugYaml "config:" envConfig

      -- load git info
      gitInfo <- loadGitInfo configBranch

      coloredPrint Blue (ppConfig  envConfig)
      coloredPrint Blue (ppGitInfo gitInfo)
      -- process changelogs
      projectRoot <- liftIO $ splitPwdBy . extractProjectNameFromUrl $ gitRemoteUrl gitInfo
      case projectRoot of
        Just dir -> withDir dir $ processChangelogs gitInfo
        Nothing  -> processChangelogs gitInfo
