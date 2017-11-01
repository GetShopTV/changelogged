module CheckLog.Common where

import Turtle

import Data.Text (Text)
import qualified Data.Text as T

import System.Console.ANSI

import qualified Control.Foldl as Fold

import Types

changelogIsUp :: Text -> Mode -> Part -> Text -> Text -> IO Bool
changelogIsUp item mode part message changelog = do
  grepLen <- fold (inproc "grep" [item, changelog] empty) Fold.length
  case grepLen of
    0 -> do
      printf ("- "%s%" ") (showText mode)
      coloredPrint Cyan item
      case part of
        Project -> printf (" id missing in changelog: "%s%".\n") message
        API -> printf (" id missing in API changelog: "%s%".\n") message
      return False
    _ -> return True

commitMessage :: Mode -> Text -> IO Text
commitMessage _ "" = return ""
commitMessage mode commit = do
  raw <- strict $ inproc "grep" ["^\\s"] $
                    inproc "sed" ["-n", sedString] $
                      inproc "git" ["show", commit] empty
  return $ T.stripEnd $ T.stripStart raw
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
      inproc "grep" ["-v", "Merge branch"] (inproc "git" ["log", "--oneline", "--first-parent", T.stripEnd latestGitTag <> "..HEAD"] empty)
  return $ T.stripEnd tmpFile
  where
    process = fromText . T.stripEnd
