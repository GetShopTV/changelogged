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
bumpPart :: Text -> TaggedFile -> IO ()
bumpPart version file@TaggedFile{..} = do
    printf ("- Updating version for "%fp%"\n") taggedFilePath
    case extension taggedFilePath of
      Just "hs" -> bumpHS file version
      Just "json" -> bumpJSON file version
      Just "cabal" -> bumpCabal file version
      _ -> coloredPrint Red ("ERROR: Didn't bump version in " <> showPath taggedFilePath <> " : only .hs and .json supported, sorry.")

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
