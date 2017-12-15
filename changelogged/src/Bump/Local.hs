module Bump.Local where

import Turtle
import Prelude hiding (FilePath, log)

import qualified Control.Foldl as Fold

import Data.Maybe (fromMaybe)
import Data.Text (Text)

import System.Console.ANSI (Color(..))

import Types
import Utils
import Pure
import Bump.Common

-- |Get current local version.
currentLocalVersion :: TaggedFile -> IO Text
currentLocalVersion TaggedFile{..} = do
  ver <- case extension taggedFilePath of
    Just "json" ->
      fold (inproc "egrep" ["-o", "[0-9][0-9.]*"] (inproc "egrep" ["\"" <> taggedFileVariable <> "\": \"[0-9][0-9.]*\""] (input taggedFilePath))) Fold.head
    Just "hs" ->
      fold (inproc "egrep" ["-o", "[0-9][0-9.]*"] (inproc "egrep" [taggedFileVariable <> "\\s+=\\s+\"[0-9][0-9.]*\""] (input taggedFilePath))) Fold.head
    _ -> do
      coloredPrint Red ("ERROR: invalid indicator file " <> showPath taggedFilePath <> " : only .hs and .json supported, sorry.")
      return (Just "")
  return $ lineToText $ fromMaybe "0.0.0.0.0" ver

-- |Generate new local version.
generateLocalVersion :: Level -> TaggedFile -> IO Text
generateLocalVersion lev indicator = do
  current <- currentLocalVersion indicator
  -- This print must not be here but I think it's better than throw current vrsion to main.
  printf ("Version: "%s%" -> ") current
  coloredPrint Yellow (new current <> "\n")
  return (new current)
  where
    new current = bump (delimited current) lev

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
