module Git where

import Data.Text (Text)
import qualified Data.Text as Text

import Turtle

import Types

latestGitTag :: Text -> IO Text
latestGitTag repl = do
  ver <- strict $ inproc "cut" ["-c", "2-"] (inproc "git" ["describe", "--tags", "origin/master"] empty)
  return $ case ver of
    "" -> repl
    _ -> Text.stripEnd ver

getLink :: IO Text
getLink = do
  raw <- strict $ inproc "git" ["remote", "get-url", "origin"] empty
  return $ case Text.stripSuffix ".git\n" raw of
    Just link -> link
    Nothing -> Text.stripEnd raw

-- Extract latest history and origin link from git.
gitData :: Bool -> IO Git
gitData start = do
  curDir <- pwd
  tmpFile <- with (mktempfile curDir "tmp_") return
  latestTag <- ("v" <> ) <$> latestGitTag ""
  link <- getLink
  if start || (latestTag == "")
    then liftIO $ append tmpFile $
      inproc "grep" ["-v", "Merge branch"] (inproc "git" ["log", "--oneline", "--first-parent"] empty)
    else liftIO $ append tmpFile $
      inproc "grep" ["-v", "Merge branch"] (inproc "git" ["log", "--oneline", "--first-parent", Text.stripEnd latestTag <> "..HEAD"] empty)
  return $ Git tmpFile link
