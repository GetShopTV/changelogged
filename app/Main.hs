module Main where

import Turtle hiding (FilePath)

import Data.Maybe (fromMaybe)
import Data.Text (unpack, pack)

import System.Console.ANSI (Color(..))

import Changelogged.EntryPoint

import Changelogged.Common
import Changelogged.Config
import Changelogged.Options

prepareConfig :: FilePath -> Options -> IO Config
prepareConfig configPath Options{..} = do
  -- load config file (or default config)
  config'@Config{..} <- fromMaybe defaultConfig <$> loadConfig configPath
  -- ignore all changelogs by default
  let changelogs = map changelogChangelog configChangelogs
      config = config' {configChangelogs = map (\cc -> cc {changelogIgnoreFiles = Just changelogs <> changelogIgnoreFiles cc}) configChangelogs}
  return config

main :: IO ()
main = do
  -- parse command line options
  opts@Options{..} <- parseOptions

  let configPath = fromMaybe ".changelogged.yaml" (unpack . showPath <$> optConfigPath)
  config@Config{..} <- prepareConfig configPath opts

  runInAppl opts config $ if optVersion
    then versionP changeloggedVersion
    else do
      debugYaml "parsed options:" opts
      debugYaml "config:" config

      -- load git info
      gitInfo <- loadGitInfo configBranch
      if config == defaultConfig
        then coloredPrint Blue "Using default config.\n"
        else coloredPrint Blue ("Configuration file: " <> pack configPath <> "\n")
      
      coloredPrint Blue (ppConfig  config)
      coloredPrint Blue (ppGitInfo gitInfo)
      -- process changelogs
      projectRoot <- liftIO $ splitPwdBy . extractProjectNameFromUrl $ gitRemoteUrl gitInfo
      case projectRoot of
        Just dir -> withDir dir $ processChangelogs gitInfo
        Nothing -> processChangelogs gitInfo
