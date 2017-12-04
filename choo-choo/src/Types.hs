-- Types and helpers for choo-choo util.

module Types where

import Data.Char (toLower)
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as Text

import System.Console.ANSI
import Options.Applicative hiding (switch)

import Prelude hiding (FilePath)
import Turtle hiding (option)

data Part = API | Project
  
data Level = App | Major | Minor | Fix | Doc
  deriving (Show, Enum, Bounded)

data Mode = PR | Commit

instance Show Mode where
  show PR = "Pull request"
  show Commit = "Single commit"

showText :: Show a => a -> Text
showText = Text.pack . show

coloredPrint :: Color -> Text -> IO ()
coloredPrint color line = do
  setSGR [SetColor Foreground Vivid color]
  printf s line
  setSGR [Reset]

data WarningFormat
  = WarnSimple
  | WarnSuggest
  deriving (Enum, Bounded)

instance Show WarningFormat where
  show WarnSimple  = "simple"
  show WarnSuggest = "suggest"

availableWarningFormats :: [WarningFormat]
availableWarningFormats = [minBound..maxBound]

availableWarningFormatsStr :: String
availableWarningFormatsStr
  = "(" <> intercalate " " (map (map toLower . show) availableWarningFormats) <> ")"

readWarningFormat :: ReadM WarningFormat
readWarningFormat = eitherReader (r . map toLower)
  where
    r "simple"  = Right WarnSuggest
    r "suggest" = Right WarnSuggest
    r fmt = Left $
         "Unknown warning format: " <> show fmt <> ".\n"
      <> "Use one of " <> availableWarningFormatsStr <> ".\n"

data Options = Options
  { optPackages      :: Maybe [Text]
  , optPackagesLevel :: Maybe Level
  , optApiLevel      :: Maybe Level
  , optFormat        :: WarningFormat
  , optNoCheck       :: Bool
  , optFromBC        :: Bool
  , optForce         :: Bool
  }

-- |
-- >>> availableLevels
-- [App,Major,Minor,Fix,Doc]
availableLevels :: [Level]
availableLevels = [minBound..maxBound]

-- |
-- >>> availableLevelsStr
-- "(app major minor fix doc)"
availableLevelsStr :: String
availableLevelsStr
  = "(" <> intercalate " " (map (map toLower . show) availableLevels) <> ")"

readLevel :: ReadM Level
readLevel = eitherReader (r . map toLower)
  where
    r "app"   = Right App
    r "major" = Right Major
    r "minor" = Right Minor
    r "fix"   = Right Fix
    r "doc"   = Right Doc
    r lvl = Left $
         "Unknown level of changes: " <> show lvl <> ".\n"
      <> "Use one of " <> availableLevelsStr <> ".\n"

parser :: Parser Options
parser = Options
  <$> optional packages
  <*> optional packagesLevel
  <*> optional apiLevel
  <*> warningFormat
  <*> switch  "no-check"  'c' "Do not check changelogs."
  <*> switch  "from-bc"  'e' "Check changelogs from start of project."
  <*> switch  "force"  'f' "Bump version even if changelogs are outdated. Cannot be mixed with -c."
  where
    packages = Text.words <$> optText
      "packages" 'p' "List of packages to bump (separated by space)."
    packagesLevel = option readLevel $
         long "level"
      <> short 'l'
      <> help ("Level of changes (for packages). One of " <> availableLevelsStr)
    apiLevel = option readLevel $
         long "api-level"
      <> short 'a'
      <> help ("Level of changes (for API). One of " <> availableLevelsStr)
    warningFormat = option readWarningFormat $
         long "format"
      <> help ("Warning format. One of " <> availableWarningFormatsStr)
      <> value WarnSimple
      <> showDefault

welcome :: Description
welcome = Description $ "---\n"
        <> "This tool can check your changelogs and bump versions in project.\n"
        <> "It assumes to be run in root directory of project and that changelog is here.\n"
        <> "You can specify these levels of changes: app, major, minor, fix, doc.\n"
        <> "It can infer version from changelog.\n"
        <> "But it will refuse to do it if it's not sure changelogs are up to date."
