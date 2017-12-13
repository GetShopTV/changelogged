module Git where

import qualified Control.Foldl as Fold

import Data.Char (isDigit)
import Data.Either.Combinators (fromRight)
import Data.Text (Text)
import qualified Data.Text as Text

import Turtle

import Types

-- |Get latest git tag in origin/master if present.
latestGitTag :: Text -> IO Text
latestGitTag repl = do
  ver <- fold (fromRight "" <$> inprocWithErr "git" ["describe", "--tags", "origin/master"] empty) Fold.head
  return $ case ver of
    Nothing -> repl
    Just v -> lineToText v

-- |Get link to origin.
getLink :: IO Text
getLink = do
  raw <- strict $ inproc "git" ["remote", "get-url", "origin"] empty
  return $ case Text.stripSuffix ".git\n" raw of
    Just link -> link
    Nothing -> Text.stripEnd raw

-- |Extract latest history and origin link from git through temporary file and store it in '@Git@'.
gitData :: Bool -> IO Git
gitData start = do
  curDir <- pwd
  tmpFile <- with (mktempfile curDir "tmp_") return
  latestTag <- latestGitTag ""
  link <- getLink
  if start || (latestTag == "")
    then liftIO $ append tmpFile $
      inproc "grep" ["-v", "Merge branch"] (inproc "git" ["log", "--oneline", "--first-parent"] empty)
    else liftIO $ append tmpFile $
      inproc "grep" ["-v", "Merge branch"] (inproc "git" ["log", "--oneline", "--first-parent", Text.stripEnd latestTag <> "..HEAD"] empty)
  return $ Git tmpFile link (version latestTag)
  where
    version tag = case tag of
      "" -> "0.0.0.0.0"
      v -> Text.dropWhile (not . isDigit) v
