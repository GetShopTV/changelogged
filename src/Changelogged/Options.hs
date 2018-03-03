module Changelogged.Options where

import Data.Char (toLower)

import Options.Applicative
import Turtle hiding (option, switch)

import Changelogged.Types

availableWarningFormats :: [WarningFormat]
availableWarningFormats = [minBound..maxBound]

availableWarningFormatsStr :: String
availableWarningFormatsStr
  = "(" <> unwords (map (map toLower . show) availableWarningFormats) <> ")"

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
availableLevelsStr
  = "(" <> unwords (map (map toLower . show) availableLevels) <> ")"

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
  <$> optional packagesLevel
  <*> optional apiLevel
  <*> warningFormat
  <*> longSwitch  "no-check"      "Do not check changelogs."
  <*> longSwitch  "bump-versions" "Bump versions according to unreleased changelog entries."
  <*> longSwitch  "from-bc"       "Check changelogs from start of project."
  <*> longSwitch  "force"         "Bump version even if changelogs are outdated. Cannot be mixed with --no-check."
  <*> longSwitch  "write"         "Write changelog suggestions to changelog directly. Available with --format suggest."
  where
    longSwitch name description = switch $
         long name
      <> help description
    packagesLevel = option readLevel $
         long "level"
      <> metavar "CHANGE_LEVEL"
      <> help ("Level of changes (for packages). One of " <> availableLevelsStr)
    apiLevel = option readLevel $
         long "api-level"
      <> metavar "CHANGE_LEVEL"
      <> help ("Level of changes (for API). One of " <> availableLevelsStr)
    warningFormat = option readWarningFormat $
         long "format"
      <> metavar "FORMAT"
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

data Options = Options
  { -- |Explicit level of changes for files with common versioning.
    optPackagesLevel   :: Maybe Level
  -- |Explicit level of changes in API.
  , optApiLevel        :: Maybe Level
  -- |Output formatting.
  , optFormat          :: WarningFormat
  -- |Do not check changelogs.
  , optNoCheck         :: Bool
  -- | Bump versions according to unreleased changelog entries.
  , optBumpVersions    :: Bool
  -- |Check changelogs from start of project.
  , optFromBC          :: Bool
  -- |Bump versions even if changelogs are outdated.
  , optForce           :: Bool
  -- |Write suggestions to changelog directly.
  , optWrite           :: Bool
  }
