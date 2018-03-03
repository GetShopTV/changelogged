module Changelogged.Options where

import Data.Char (toLower)
import Data.List (intercalate)
import Data.Monoid ((<>))

import Options.Applicative
import Turtle (Description(..))

import Changelogged.Types

availableWarningFormats :: [WarningFormat]
availableWarningFormats = [minBound..maxBound]

availableWarningFormatsStr :: String
availableWarningFormatsStr = prettyPossibleValues availableWarningFormats

readWarningFormat :: ReadM WarningFormat
readWarningFormat = eitherReader (r . map toLower)
  where
    r "simple"  = Right WarnSimple
    r "suggest" = Right WarnSuggest
    r fmt = Left $
         "Unknown warning format: " <> show fmt <> ".\n"
      <> "Use one of " <> availableWarningFormatsStr <> ".\n"

-- |
-- >>> availableLevels
-- [App,Major,Minor,Fix,Doc]
availableLevels :: [Level]
availableLevels = [minBound..maxBound]

-- |
-- >>> availableLevelsStr
-- "(app major minor fix doc)"
availableLevelsStr :: String
availableLevelsStr = prettyPossibleValues availableLevels

prettyPossibleValues :: Show a => [a] -> String
prettyPossibleValues xs = case reverse xs of
  []  -> "none"
  [y] -> prettyValue y
  (y:ys) -> intercalate ", " (map prettyValue (reverse ys)) <> " or " <> prettyValue y
  where
    prettyValue v = "'" <> map toLower (show v) <> "'"

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
      <> "Should be " <> availableLevelsStr <> ".\n"

parser :: Parser Options
parser = Options
  <$> warningFormat
  <*> longSwitch  "update-changelog" "Prepend missing entries to changelogs. Available with --format=suggest."
  <*> longSwitch  "bump-versions"    "Bump versions according to change level."
  <*> optional packagesLevel
  <*> optional apiLevel
  <*> hiddenSwitch "from-bc"  "Check changelogs for the entire history of the project."
  <*> hiddenSwitch "force"    "Bump versions even when changelogs are outdated."
  <*> hiddenSwitch "no-check" "Do not check changelogs."
  where
    longSwitch name description = switch $
         long name
      <> help description

    hiddenSwitch name description = switch $
         long name
      <> help description
      <> hidden

    packagesLevel = option readLevel $
         long "level"
      <> metavar "CHANGE_LEVEL"
      <> help ("Level of changes (for packages). CHANGE_LEVEL can be " <> availableLevelsStr <> ".")
      <> hidden
    apiLevel = option readLevel $
         long "api-level"
      <> metavar "CHANGE_LEVEL"
      <> help ("Level of changes (for API). CHANGE_LEVEL can be " <> availableLevelsStr <> ".")
      <> hidden
    warningFormat = option readWarningFormat $
         long "format"
      <> metavar "FORMAT"
      <> help ("Missing entries report format. FORMAT can be " <> availableWarningFormatsStr <> ".")
      <> value WarnSimple
      <> showDefault

welcome :: Description
welcome = Description "changelogged - Changelog Manager for Git Projects"

data Options = Options
  {
  -- |Output formatting.
    optFormat          :: WarningFormat
  -- | Write suggestions to changelog directly.
  , optUpdateChangelog :: Bool
  -- | Bump versions according to unreleased changelog entries.
  , optBumpVersions    :: Bool
  -- |Explicit level of changes for files with common versioning.
  , optPackagesLevel   :: Maybe Level
  -- |Explicit level of changes in API.
  , optApiLevel        :: Maybe Level
  -- |Check changelogs from start of project.
  , optFromBC          :: Bool
  -- |Bump versions even if changelogs are outdated.
  , optForce           :: Bool
  -- |Do not check changelogs.
  , optNoCheck         :: Bool
  }
