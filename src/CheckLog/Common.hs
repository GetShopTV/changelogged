module CheckLog.Common where

import Prelude hiding (FilePath)
import Turtle

import qualified Control.Foldl as Fold

import Data.Text (Text)
import qualified Data.Text as Text

import System.Console.ANSI (Color(..))

import Types
import Utils
import Pure

-- |Check if commit/pr is present in changelog. Return '@True@' if present.
changelogIsUp :: WarningFormat -> Bool -> Text -> Text -> Mode -> Text -> FilePath -> IO Bool
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

-- |Ignore commits which only affect '.md' files
noMarkdown :: Text -> IO Bool
noMarkdown commit = do
  statCommit <- fold (inproc "git" ["show", "--stat", commit] empty) Fold.list
  chLogUpdated <- fold
    (grep (has $ text ".md ")
      (select statCommit)) countLines
  onlyChLogUpdated <- fold
    (grep (has $ text "|")
      (select statCommit)) countLines
  return $ chLogUpdated /= onlyChLogUpdated

-- |
-- >>> warnMissing "#13" PR "Add new stuff"
-- - Pull request #13 is missing in changelog: Add new stuff.
--
-- >>> warnMissing "9e14840" Commit "Add new stuff"
-- - Single commit 9e14840 is missing in changelog: Add new stuff.
warnMissing :: Text -> Mode -> Text -> IO ()
warnMissing item mode message = do
  printf ("- "%s%" ") (showText mode)
  coloredPrint Cyan item
  printf (" is missing: "%s%".\n") message

-- |
-- >>> prLink "#13"
-- "https://github.com/GetShopTV/getshoptv/pull/13"
prLink :: Text -> Text -> Text
prLink link num = link <> "/pull/" <> Text.drop 1 num

-- |
-- >>> commitLink "9e14840"
-- "https://github.com/GetShopTV/getshoptv/commit/9e14840"
commitLink :: Text -> Text -> Text
commitLink link sha = link <> "/commit/" <> sha

-- |
-- >>> suggestMissing "#13" PR "Add new stuff"
-- - Add new stuff (see [#13](https://github.com/GetShopTV/getshoptv/pull/13));
--
-- >>> suggestMissing "9e14840" Commit "Add new stuff"
-- - Add new stuff (see [`9e14840`](https://github.com/GetShopTV/getshoptv/commit/9e14840));
suggestMissing :: Text -> Text -> Mode -> Text -> IO ()
suggestMissing link item mode message = do
  printf ("- "%s%" (see ") message
  case mode of
    PR -> do
      coloredPrint Cyan ("[" <> item <> "]")
      printf ("("%s%")") (prLink link item)
    Commit -> do
      coloredPrint Cyan ("[`" <> item <> "`]")
      printf ("("%s%")") (commitLink link item)
  printf (");\n")

-- |Add generated suggestion directly to changelog.
addMissing :: Text -> Text -> Mode -> Text -> FilePath -> IO ()
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
commitMessage :: Mode -> Text -> IO Text
commitMessage _ "" = return ""
commitMessage mode commit = do
  summary <- fold (inproc "git" ["show", commit] empty) Fold.list
  return $ Text.stripStart $ lineToText $ case mode of
    PR -> summary !! 7
    Commit -> summary !! 4
