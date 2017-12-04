module CheckLog.Common where

import Turtle

import Data.Text (Text)
import qualified Data.Text as Text

import System.Console.ANSI

import qualified Control.Foldl as Fold

import Types

changelogIsUp :: WarningFormat -> Text -> Mode -> Part -> Text -> Text -> IO Bool
changelogIsUp fmt item mode _part message changelog = do
  grepLen <- fold (inproc "grep" [item, changelog] empty) Fold.length
  case grepLen of
    0 -> do
      case fmt of
        WarnSimple  -> warnMissing item mode message
        WarnSuggest -> suggestMissing item mode message
      return False
    _ -> return True

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
  printf (" is missing in changelog: "%s%".\n") message

-- |
-- >>> prLink "#13"
-- "https://github.com/GetShopTV/getshoptv/pull/13"
prLink :: Text -> Text
prLink num = "https://github.com/GetShopTV/getshoptv/pull/" <> Text.drop 1 num

-- |
-- >>> commitLink "9e14840"
-- "https://github.com/GetShopTV/getshoptv/commit/9e14840"
commitLink :: Text -> Text
commitLink sha = "https://github.com/GetShopTV/getshoptv/commit/" <> sha

-- |
-- >>> suggestMissing "#13" PR "Add new stuff"
-- - Add new stuff (see [#13](https://github.com/GetShopTV/getshoptv/pull/13));
--
-- >>> suggestMissing "9e14840" Commit "Add new stuff"
-- - Add new stuff (see [`9e14840`](https://github.com/GetShopTV/getshoptv/commit/9e14840));
suggestMissing :: Text -> Mode -> Text -> IO ()
suggestMissing item mode message = do
  printf ("- "%s%" (see ") message
  case mode of
    PR -> do
      coloredPrint Cyan ("[" <> item <> "]")
      printf ("("%s%")") (prLink item)
    Commit -> do
      coloredPrint Cyan ("[`" <> item <> "`]")
      printf ("("%s%")") (commitLink item)
  printf ");\n"

commitMessage :: Mode -> Text -> IO Text
commitMessage _ "" = return ""
commitMessage mode commit = do
  raw <- strict $ inproc "grep" ["^\\s"] $
                    inproc "sed" ["-n", sedString] $
                      inproc "git" ["show", commit] empty
  return $ Text.stripEnd $ Text.stripStart raw
  where
    sedString = case mode of
      PR -> "8p"
      Commit -> "5p"

gitLatestHistory :: Bool -> IO Text
gitLatestHistory start = do
  tmpFile <- strict $ inproc "mktemp" [] empty
  latestGitTag <- strict $ inproc "git" ["describe", "--tags", "origin/master"] empty
  if start
    then liftIO $ append (process tmpFile) $
      inproc "grep" ["-v", "Merge branch"] (inproc "git" ["log", "--oneline", "--first-parent"] empty)
    else liftIO $ append (process tmpFile) $
      inproc "grep" ["-v", "Merge branch"] (inproc "git" ["log", "--oneline", "--first-parent", Text.stripEnd latestGitTag <> "..HEAD"] empty)
  return $ Text.stripEnd tmpFile
  where
    process = fromText . Text.stripEnd
