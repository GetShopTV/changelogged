module Bump.Project where

import Turtle
import Prelude hiding (FilePath, log)

import Data.Text (Text)

import System.Console.ANSI (Color(..))

import Types
import Pure
import Git
import Utils
import Bump.Common

bumpPart :: Text -> (FilePath, Variable) -> IO ()
bumpPart version (file, var) = do
    printf ("- Updating version for "%fp%"\n") file
    case extension file of
      Just "hs" -> bumpHS file version var
      Just "json" -> bumpJSON file version var
      _ -> coloredPrint Red ("ERROR: Didn't bump version in " <> showPath file <> " : only .hs and .json supported, sorry.")

bumpPackage :: Text -> Text -> IO ()
bumpPackage version packageName = do
  printf ("- Updating version for "%s%"\n") packageName
  stdout (inproc "sed" ["-i", "-r", expr, file] empty)
  where
    packageFile = fromText packageName
    file = format fp $ packageFile </> packageFile <.> "cabal"
    expr = "s/(^version:[^0-9]*)[0-9][0-9.]*/\\1" <> version <> "/"

bumpPackages :: Text -> [Text] -> IO ()
bumpPackages version packages = do
  curVersion <- currentVersion

  printf ("Version: "%s%" -> ") curVersion
  coloredPrint Yellow (version <> "\n")

  printf ("Updating packages version to "%s%"\n") version
  mapM_ (bumpPackage version) packages

currentVersion :: IO Text
currentVersion = latestGitTag "0.0.0.0.0"

generateVersion :: Level -> IO Text
generateVersion lev = do
  current <- currentVersion
  return $ bump (delimited current) lev

generateVersionByChangelog :: Bool -> FilePath -> IO (Maybe Text)
generateVersionByChangelog True _ = do
  coloredPrint Yellow "You are bumping version with no explicit version modifiers and changelog checks. It can result in anything. Please retry.\n"
  return Nothing
generateVersionByChangelog False changelogFile = do
  versionedChanges <- getChangelogEntries changelogFile
  case versionedChanges of
    Just lev -> Just <$> generateVersion lev
    Nothing -> do
      coloredPrint Yellow ("WARNING: keep old version since " <> showPath changelogFile <> " apparently does not contain any new entries.\n")
      return Nothing
