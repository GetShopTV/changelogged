module Bump.Project where

import Turtle
import Prelude hiding (FilePath, log)

import Data.Text (Text)
import qualified Data.Text as T

import System.Console.ANSI (Color(..))

import Types
import Utils
import Bump.Common

bumpPart :: Text -> (Text, Text) -> IO ()
bumpPart version (file, var) = do
    printf ("- Updating version for "%s%"\n") file
    case snd (T.breakOnEnd "." file) of
      "hs" -> bumpHS file version var
      "json" -> bumpJSON file version var
      _ -> coloredPrint Red ("ERROR: Didn't bump version in " <> file <> " : only .hs and .json supported, sorry.")

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
currentVersion = do
  ver <- strict $ inproc "cut" ["-c", "2-"] (inproc "git" ["describe", "--tags", "origin/master"] empty)
  return $ T.stripEnd ver

generateVersion :: Level -> IO Text
generateVersion lev = do
  current <- currentVersion
  return $ bump (delimited current) lev

generateVersionByChangelog :: Bool -> Text -> IO (Maybe Text)
generateVersionByChangelog True _ = do
  coloredPrint Yellow "You are bumping version with no explicit version modifiers and changelog checks. It can result in anything. Please retry.\n"
  return Nothing
generateVersionByChangelog False changelogFile = do
  (major, minor, fixes, docs) <- getChangelogEntries changelogFile

  case major of
    0 -> case minor of
      0 -> case fixes of
        0 -> case docs of
          0 -> do
            coloredPrint Yellow ("WARNING: keep old version since " <> changelogFile <> " apparently does not contain any new entries.\n")
            return Nothing
          _ -> Just <$> generateVersion Doc
        _ -> Just <$> generateVersion Fix
      _ -> Just <$> generateVersion Minor
    _ -> Just <$> generateVersion Major
