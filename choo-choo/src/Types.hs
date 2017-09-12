-- Types and helpers for choo-choo util.

module Types where

import Data.Text (Text, pack)

import Prelude hiding (FilePath)
import Turtle

import System.Console.ANSI

coloredPrint :: Color -> Text -> IO ()
coloredPrint color line = do
  setSGR [SetColor Foreground Vivid color]
  printf s line
  setSGR [Reset]

data Part = API | Project
data Level = App | Major | Minor | Fix | Doc
data Mode = PR | Commit

instance Show Mode where
  show PR = "Pull request"
  show Commit = "Single commit"

showText :: Show a => a -> Text
showText = pack . show

changelogFile :: Text
changelogFile = "CHANGELOG.md"

apiChangelogFile :: Text
apiChangelogFile = "API_CHANGELOG.md"

levelFromText :: Text -> Level
levelFromText "app" = App
levelFromText "App" = App
levelFromText "APP" = App
levelFromText "major" = Major
levelFromText "Major" = Major
levelFromText "MAJOR" = Major
levelFromText "minor" = Minor
levelFromText "Minor" = Minor
levelFromText "MINOR" = Minor
levelFromText "fix" = Fix
levelFromText "Fix" = Fix
levelFromText "FIX" = Fix
levelFromText "doc" = Doc
levelFromText "Doc" = Doc
levelFromText "DOC" = Doc
levelFromText _ = error "Unsupported level of changes. See supported with -h or --help."


parser :: Parser (Maybe Text, Maybe Text, Maybe Text, Bool, Bool, Bool)
parser = (,,,,,) <$> optional (optText "packages" 'p' "List of packages to bump.")
                 <*> optional (optText "level" 'l' "Level of changes.")
                 <*> optional (optText "api" 'a' "Level of changes in API.")
                 <*> switch  "no-check"  'c' "Do not check changelogs."
                 <*> switch  "from-bc"  'e' "Check changelogs from start of project."
                 <*> switch  "force"  'f' "Bump version even if changelogs are outdated. Cannot be mixed with -c."

welcome :: Description
welcome = Description $ "---\n"
        <> "This script can check your changelogs and bump versions in project.\n"
        <> "It assumes to be run in root directory of project and that changelog is here.\n"
        <> "You can specify these levels of changes: app, major, minor, fix, doc.\n"
        <> "It can infer version from changelog.\n"
        <> "But it will refuse to do it if it's not sure changelogs are up to date."
