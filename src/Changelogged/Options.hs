{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Changelogged.Options where

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Reader

import Data.Char (toLower)
import Data.List (intercalate)
import Data.Monoid ((<>))
import Data.String.Conversions (cs)

import Options.Applicative
import qualified Turtle

import Filesystem.Path.CurrentOS (valid, fromText)

import Changelogged.Types

newtype Appl a = Appl { runAppl :: ReaderT Options IO a }
  deriving (Functor, Applicative, Monad, MonadReader Options, MonadIO, MonadBase IO, MonadThrow, MonadCatch)

runInAppl :: Options -> Appl a -> IO a
runInAppl opts r = runReaderT (runAppl r) opts

-- |
-- >>> availableWarningFormats
-- [simple,suggest]
availableWarningFormats :: [WarningFormat]
availableWarningFormats = [minBound..maxBound]

-- |
-- >>> availableWarningFormatsStr
-- "'simple' or 'suggest'"
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
-- "'app', 'major', 'minor', 'fix' or 'doc'"
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

readFilePath :: ReadM Turtle.FilePath
readFilePath = eitherReader r
  where
    r filePathString = if valid $ fromText $ cs filePathString
      then Right (fromText $ cs filePathString)
      else Left ("Invalid file path " <> filePathString <> ".\n")

parser :: Parser Options
parser = Options
  <$> warningFormat
  <*> longSwitch "update-changelog"
        "Add missing entries to changelogs (works with --format=suggest)."
  <*> longSwitch "bump-versions" "Bump versions in version files."
  <*> optional changesLevel
  <*> hiddenSwitch "from-bc"
        "Look for missing changelog entries from the start of the project."
  <*> hiddenSwitch "force" "Bump versions ignoring possibly outdated changelogs."
  <*> hiddenSwitch "no-check" "Do not check if changelogs have any missing entries."
  <*> hiddenSwitch "no-colors" "Print all messages in standard terminal color."
  <*> optional targetChangelog
  <*> optional configPath
  <*> hiddenSwitch "version" "Print version."
  where
    longSwitch name description = switch $
         long name
      <> help description

    hiddenSwitch name description = switch $
         long name
      <> help description
      <> hidden

    changesLevel = option readLevel $
         long "level"
      <> metavar "CHANGE_LEVEL"
      <> help (unlines
           [ "Level of changes (to override one inferred from changelog)."
           , "CHANGE_LEVEL can be " <> availableLevelsStr <> "."
           ])
      <> hidden
    warningFormat = option readWarningFormat $
         long "format"
      <> metavar "FORMAT"
      <> help ("Format for missing changelog entry warnings. FORMAT can be " <> availableWarningFormatsStr <> ".")
      <> value WarnSimple
      <> showDefault
    targetChangelog = argument readFilePath $
         metavar "TARGET_CHANGELOG"
      <> help ("Path to target changelog.")
    configPath = option readFilePath $
         long "config"
      <> metavar "changelogged.yaml config file location"
      <> help ("Path to config file.")

welcome :: Turtle.Description
welcome = Turtle.Description "changelogged - Changelog Manager for Git Projects"

-- | Command line options for @changelogged@.
data Options = Options
  { -- | Format for missing changelog entry warnings.
    optFormat          :: WarningFormat
    -- | Add missing changelog entries to changelog files.
  , optUpdateChangelog :: Bool
    -- | Bump versions in version files.
  , optBumpVersions    :: Bool
    -- | Level of changes (to override one inferred from changelogs).
  , optChangeLevel     :: Maybe Level
    -- | Look for missing changelog entries from the start of the project.
  , optFromBC          :: Bool
    -- | Bump versions ignoring possibly outdated changelogs.
  , optForce           :: Bool
    -- | Do not check if changelogs have any missing entries.
  , optNoCheck         :: Bool
    -- | Print all texts in standard terminal color.
  , optNoColors        :: Bool
    -- | Check exactly one target changelog.
  , optTargetChangelog :: Maybe Turtle.FilePath
    -- | Use specified config file.
  , optConfigPath      :: Maybe Turtle.FilePath
    -- | Print version
  , optVersion         :: Bool
  }

-- | Parse command line options.
parseOptions :: IO Options
parseOptions = Turtle.options welcome parser
