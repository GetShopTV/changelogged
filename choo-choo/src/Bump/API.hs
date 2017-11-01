module Bump.API where

import Turtle
import Prelude hiding (FilePath, log)

import Data.Text (Text)
import qualified Data.Text as T

import System.Console.ANSI (Color(..))

import Types
import Utils
import Bump.Common

currentAPIVersion :: (Text, Text) -> IO Text
currentAPIVersion (swagger, var) = do
  ver <- case snd (T.breakOnEnd "." swagger) of
    "json" -> strict $ inproc "egrep" ["-o", "[0-9][0-9.]*"] (inproc "egrep" ["\"" <> var <> "\": \"[0-9][0-9.]*\"", swagger] empty)
    "hs" -> strict $ inproc "egrep" ["-o", "[0-9][0-9.]*"] (inproc "egrep" [var <> " = \"[0-9][0-9.]*\"", swagger] empty)
    _ -> do
      coloredPrint Red ("ERROR: invalid indicator file " <> swagger <> " : only .hs and .json supported, sorry.")
      return ""
  return $ T.stripEnd ver

generateAPIVersion :: Level -> (Text, Text) -> IO Text
generateAPIVersion lev swagger = do
  current <- currentAPIVersion swagger
  return $ bump (delimited current) lev

generateAPIVersionByChangelog :: Bool -> (Text, Text) -> Text -> IO (Maybe Text)
generateAPIVersionByChangelog True _ _ = do
  coloredPrint Yellow "You are bumping API version with no explicit version modifiers and changelog checks. It can result in anything. Please retry.\n"
  return Nothing
generateAPIVersionByChangelog False swagger changelogFile = do
  (major, minor, fixes, docs) <- getChangelogEntries changelogFile

  case major of
    0 -> case minor of
      0 -> case fixes of
        0 -> case docs of
          0 -> do
            coloredPrint Yellow ("WARNING: keep old API version since " <> changelogFile <> " apparently does not contain any new entries.\n")
            return Nothing
          _ -> Just <$> generateAPIVersion Doc swagger
        _ -> Just <$> generateAPIVersion Fix swagger
      _ -> Just <$> generateAPIVersion Minor swagger
    _ -> Just <$> generateAPIVersion Major swagger

bumpAPIPart :: Text -> (Text, Text) -> IO ()
bumpAPIPart version (file, var) = do
    printf ("- Updating API version for "%s%"\n") file
    case snd (T.breakOnEnd "." file) of
      "hs" -> bumpHS file version var
      "json" -> bumpJSON file version var
      _ -> coloredPrint Red ("ERROR: Didn't bump API version in " <> file <> " : only .hs and .json supported, sorry.")
