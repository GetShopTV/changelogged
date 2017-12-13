module Bump.Local where

import Turtle
import Prelude hiding (FilePath, log)

import Data.Text (Text)
import qualified Data.Text as T

import System.Console.ANSI (Color(..))

import Types
import Utils
import Pure
import Bump.Common

-- |Get current local version.
currentLocalVersion :: TaggedFile -> IO Text
currentLocalVersion TaggedFile{..} = do
  ver <- case extension taggedFilePath of
    Just "json" -> strict $ inproc "egrep" ["-o", "[0-9][0-9.]*"] (inproc "egrep" ["\"" <> taggedFileVariable <> "\": \"[0-9][0-9.]*\""] (input taggedFilePath))
    Just "hs" -> strict $ inproc "egrep" ["-o", "[0-9][0-9.]*"] (inproc "egrep" [taggedFileVariable <> "\\s+=\\s+\"[0-9][0-9.]*\""] (input taggedFilePath))
    _ -> do
      coloredPrint Red ("ERROR: invalid indicator file " <> showPath taggedFilePath <> " : only .hs and .json supported, sorry.")
      return ""
  return $ T.stripEnd ver

-- |Generate new local version.
generateLocalVersion :: Level -> TaggedFile -> IO Text
generateLocalVersion lev indicator = do
  current <- currentLocalVersion indicator
  return $ bump (delimited current) lev

-- |Infer new local version.
generateLocalVersionByChangelog :: Bool -> TaggedLog -> IO (Maybe Text)
generateLocalVersionByChangelog True _ = do
  coloredPrint Yellow "You are bumping API version with no explicit version modifiers and changelog checks. It can result in anything. Please retry.\n"
  return Nothing
generateLocalVersionByChangelog False TaggedLog{..} = do
  versionedChanges <- getChangelogEntries taggedLogPath
  case versionedChanges of
    Just lev -> Just <$> generateLocalVersion lev (fromJustCustom taggedLogIndicator "No file with current local version specified.")
    Nothing -> do
      coloredPrint Yellow ("WARNING: keep old API version since " <> showPath taggedLogPath <> " apparently does not contain any new entries.\n")
      return Nothing
