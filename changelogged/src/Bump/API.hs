module Bump.API where

import Turtle
import Prelude hiding (FilePath, log)

import Data.Text (Text)
import qualified Data.Text as T

import System.Console.ANSI (Color(..))

import Types
import Utils
import Bump.Common

currentAPIVersion :: (FilePath, Variable) -> IO Text
currentAPIVersion (swagger, var) = do
  ver <- case extension swagger of
    Just "json" -> strict $ inproc "egrep" ["-o", "[0-9][0-9.]*"] (inproc "egrep" ["\"" <> var <> "\": \"[0-9][0-9.]*\""] (input swagger))
    Just "hs" -> strict $ inproc "egrep" ["-o", "[0-9][0-9.]*"] (inproc "egrep" [var <> " = \"[0-9][0-9.]*\""] (input swagger))
    _ -> do
      coloredPrint Red ("ERROR: invalid indicator file " <> showPath swagger <> " : only .hs and .json supported, sorry.")
      return ""
  return $ T.stripEnd ver

generateAPIVersion :: Level -> (FilePath, Variable) -> IO Text
generateAPIVersion lev swagger = do
  current <- currentAPIVersion swagger
  return $ bump (delimited current) lev

generateAPIVersionByChangelog :: Bool -> (FilePath, Variable) -> FilePath -> IO (Maybe Text)
generateAPIVersionByChangelog True _ _ = do
  coloredPrint Yellow "You are bumping API version with no explicit version modifiers and changelog checks. It can result in anything. Please retry.\n"
  return Nothing
generateAPIVersionByChangelog False swagger changelogFile = do
  versionedChanges <- getChangelogEntries changelogFile
  case versionedChanges of
    Just lev -> Just <$> generateAPIVersion lev swagger
    Nothing -> do
      coloredPrint Yellow ("WARNING: keep old API version since " <> showPath changelogFile <> " apparently does not contain any new entries.\n")
      return Nothing

bumpAPIPart :: Text -> (FilePath, Variable) -> IO ()
bumpAPIPart version (file, var) = do
    printf ("- Updating API version for "%fp%"\n") file
    case extension file of
      Just "hs" -> bumpHS file version var
      Just "json" -> bumpJSON file version var
      _ -> coloredPrint Red ("ERROR: Didn't bump API version in " <> showPath file <> " : only .hs and .json supported, sorry.")
