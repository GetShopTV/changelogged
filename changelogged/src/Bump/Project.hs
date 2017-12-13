module Bump.Project where

import Turtle
import Prelude hiding (FilePath, log)

import Data.Text (Text)

import System.Console.ANSI (Color(..))

import Types
import Pure
import Utils
import Bump.Common

-- |Bump version in non-'.cabal' file.
bumpPart :: Text -> (FilePath, Variable) -> IO ()
bumpPart version (file, var) = do
    printf ("- Updating version for "%fp%"\n") file
    case extension file of
      Just "hs" -> bumpHS file version var
      Just "json" -> bumpJSON file version var
      _ -> coloredPrint Red ("ERROR: Didn't bump version in " <> showPath file <> " : only .hs and .json supported, sorry.")

-- |Bump version in '.cabal' file.
bumpPackage :: Text -> Text -> IO ()
bumpPackage version packageName = do
  printf ("- Updating version for "%s%"\n") packageName
  stdout (inproc "sed" ["-i", "-r", expr, file] empty)
  where
    packageFile = fromText packageName
    file = format fp $ packageFile </> packageFile <.> "cabal"
    expr = "s/(^version:[^0-9]*)[0-9][0-9.]*/\\1" <> version <> "/"

-- |Bump version in set of '.cabal' files.
bumpPackages :: Text -> [Text] -> Text -> IO ()
bumpPackages version packages curVersion = do
  printf ("Version: "%s%" -> ") curVersion
  coloredPrint Yellow (version <> "\n")

  printf ("Updating packages version to "%s%"\n") version
  mapM_ (bumpPackage version) packages

-- |Generate new version based on given level and current version.
generateVersion :: Level -> Text -> IO Text
generateVersion lev current = return $ bump (delimited current) lev

-- |Infer version from changelog.
generateVersionByChangelog :: Bool -> FilePath -> Text -> IO (Maybe Text)
generateVersionByChangelog True _ _ = do
  coloredPrint Yellow "You are bumping version with no explicit version modifiers and changelog checks. It can result in anything. Please retry.\n"
  return Nothing
generateVersionByChangelog False changelogFile curVersion = do
  versionedChanges <- getChangelogEntries changelogFile
  case versionedChanges of
    Just lev -> Just <$> generateVersion lev curVersion
    Nothing -> do
      coloredPrint Yellow ("WARNING: keep old version since " <> showPath changelogFile <> " apparently does not contain any new entries.\n")
      return Nothing
