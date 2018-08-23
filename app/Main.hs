{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import           Turtle                  hiding (FilePath)

import           Control.Monad           (when)

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
    coloredPrintIO optNoColors Magenta ("Using config file at "<> pack configPath <>"\n")
    eitherConf <- loadConfig configPath
    case eitherConf of
      Left parseError -> (die . showText) parseError
      Right conf -> return conf
    else do
      coloredPrintIO optNoColors Magenta ("Config file at "<> pack configPath <>" not found. Using default config.\n")
      return defaultConfig

main :: IO ()
main = do
  let defaultConfigPath = ".changelogged.yaml"
  -- parse command line options
  opts@Options{..} <- parseOptions

  let configPath = fromMaybe defaultConfigPath (unpack . showPath <$> optConfigPath)
  config@Config{..} <- prepareConfig configPath opts

  when optDumpConfig $ dumpConfig config defaultConfigPath

  runInAppl opts config $ if optVersion
    then versionP changeloggedVersion
    else do
      debugYaml "parsed options:" opts
      debugYaml "config:" config

      -- load git info
      gitInfo <- loadGitInfo configBranch

      coloredPrint Blue (ppConfig  config)
      coloredPrint Blue (ppGitInfo gitInfo)
      -- process changelogs
      projectRoot <- liftIO $ splitPwdBy . extractProjectNameFromUrl $ gitRemoteUrl gitInfo
      case projectRoot of
        Just dir -> withDir dir $ processChangelogs gitInfo
        Nothing  -> processChangelogs gitInfo
