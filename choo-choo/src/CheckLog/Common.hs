module CheckLog.Common where

import Prelude hiding (FilePath)
import Turtle

import Data.Text (Text)
import qualified Data.Text as Text

import System.Console.ANSI

import qualified Control.Foldl as Fold

import Types

changelogIsUp :: WarningFormat -> Text -> Text -> Mode -> Part -> Text -> Text -> IO Bool
changelogIsUp fmt link item mode _part message changelog = do
  grepLen <- fold (inproc "grep" [item, changelog] empty) Fold.length
  case grepLen of
    0 -> do
      case fmt of
        WarnSimple  -> warnMissing item mode message
        WarnSuggest -> suggestMissing link item mode message
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

getLink :: IO Text
getLink = do
  raw <- strict $ inproc "git" ["remote", "get-url", "origin"] empty
  return $ case Text.stripSuffix ".git\n" raw of
    Just link -> link
    Nothing -> Text.stripEnd raw

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

-- Extract latest history and origin link from git.
gitData :: Bool -> IO (FilePath, Text)
gitData start = do
  curDir <- pwd
  tmpFile <- with (mktempfile curDir "tmp_") return
  latestGitTag <- strict $ inproc "git" ["describe", "--tags", "origin/master"] empty
  link <- getLink
  if start
    then liftIO $ append tmpFile $
      inproc "grep" ["-v", "Merge branch"] (inproc "git" ["log", "--oneline", "--first-parent"] empty)
    else liftIO $ append tmpFile $
      inproc "grep" ["-v", "Merge branch"] (inproc "git" ["log", "--oneline", "--first-parent", Text.stripEnd latestGitTag <> "..HEAD"] empty)
  return (tmpFile, link)
