module Changelogged.CheckLog.Common where

import Prelude hiding (FilePath)
import Turtle

import qualified Control.Foldl as Fold

import Data.Text (Text)
import qualified Data.Text as Text

import System.Console.ANSI (Color(..))

import Changelogged.Types
import Changelogged.Options
import Changelogged.Utils
import Changelogged.Pure

-- |Check if commit/pr is present in changelog. Return '@True@' if present.
changelogIsUp :: WarningFormat -> Bool -> Text -> Text -> Mode -> Text -> FilePath -> Appl Bool
changelogIsUp fmt writeSug link item mode message changelog = do
  grepLen <- fold (grep (has (text item)) (input changelog)) countLines
  case grepLen of
    0 -> do
      case fmt of
        WarnSimple  -> warnMissing item mode message
        WarnSuggest -> do
          suggestMissing link item mode message
          when writeSug $ addMissing link item mode message changelog
      return False
    _ -> return True

-- |
warnMissing :: Text -> Mode -> Text -> Appl ()
warnMissing item mode message = do
  printf ("- "%s%" ") (showText mode)
  coloredPrint Cyan item
  printf (" is missing: "%s%".\n") message

-- |
-- >>> prLink "https://github.com/GetShopTV/changelogged" "#13"
-- "https://github.com/GetShopTV/changelogged/pull/13"
prLink :: Text -> Text -> Text
prLink link num = link <> "/pull/" <> Text.drop 1 num

-- |
-- >>> commitLink "https://github.com/GetShopTV/changelogged" "9e14840"
-- "https://github.com/GetShopTV/changelogged/commit/9e14840"
commitLink :: Text -> Text -> Text
commitLink link sha = link <> "/commit/" <> sha

-- |
suggestMissing :: Text -> Text -> Mode -> Text -> Appl ()
suggestMissing link item mode message = do
  printf ("- "%s%" (see ") message
  case mode of
    PR -> do
      coloredPrint Cyan $ "[" <> item <> "]"
      coloredPrint Blue $ "(" <> prLink link item <> ")"
    Commit -> do
      coloredPrint Cyan ("[`" <> item <> "`]")
      coloredPrint Blue $ "(" <> commitLink link item <> ")"
  printf ");\n"

-- |Add generated suggestion directly to changelog.
addMissing :: Text -> Text -> Mode -> Text -> FilePath -> Appl ()
addMissing link item mode message changelog = do
  currentLogs <- fold (input changelog) Fold.list
  output changelog (return $ unsafeTextToLine entry)
  append changelog (select currentLogs)
  where
    entry = prolog <> sense <> epilog
    prolog = "- " <> message <> " (see "
    sense = case mode of
        PR -> "[" <> item <> "]" <> "(" <> prLink link item <> ")"
        Commit -> "[`" <> item <> "`]" <> "(" <> commitLink link item <> ")"
    epilog = ");"

-- |Get commit message for any entry in history.
commitMessage :: Mode -> Text -> Appl Text
commitMessage _ "" = return ""
commitMessage mode commit = do
  summary <- fold (inproc "git" ["show", commit] empty) Fold.list
  return $ Text.stripStart $ lineToText $ case mode of
    PR -> summary !! 7
    Commit -> summary !! 4
