{-# LANGUAGE ScopedTypeVariables #-}
module Changelogged.Git where

import qualified Control.Foldl as Fold
import Control.Monad.Catch (catch)

import Data.Char (isDigit)
import Data.Either.Combinators (fromRight)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

import Turtle

import Changelogged.Types

-- | Get latest git tag in HEAD if present.
latestGitTag :: Text -> IO Text
latestGitTag repl = do
  ver <- fold ((fromRight "" <$> inprocWithErr "git" ["describe", "--tags", "--abbrev=0", "HEAD^"] empty) `catch` \ (_ :: ExitCode) -> empty) Fold.head
  return $ case ver of
    Nothing -> repl
    Just v -> lineToText v

-- |Get link to origin and strip '.git' to get valid url to project page.
getLink :: IO Text
getLink = do
  raw <- strict $ inproc "git" ["remote", "get-url", "origin"] empty
  return $ fromMaybe (Text.stripEnd raw) (Text.stripSuffix ".git\n" raw)

-- |Extract latest history and origin link from git through temporary file and store it in '@Git@'.
gitData :: Bool -> IO Git
gitData start = do
  curDir <- pwd
  tmpFile <- with (mktempfile curDir "tmp_") return
  latestTag <- latestGitTag ""
  link <- getLink
  hist <- if start || (latestTag == "")
    then fold (grep (invert (has (text "Merge branch"))) (inproc "git" ["log", "--oneline", "--first-parent"] empty)) Fold.list
    else fold (grep (invert (has (text "Merge branch"))) (inproc "git" ["log", "--oneline", "--first-parent", latestTag <> "..HEAD"] empty)) Fold.list
  liftIO $ append tmpFile (select hist)
  return $ Git tmpFile link (version latestTag)
  where
    version tag = case tag of
      "" -> "0.0.0.0.0"
      v -> Text.dropWhile (not . isDigit) v