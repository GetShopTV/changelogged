{-# LANGUAGE RecordWildCards #-}
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

-- | Information about the state of a git repository.
data GitInfo = GitInfo
  { gitHistory   :: [Turtle.Line]
    -- ^ A list of git commit messages.
  , gitRemoteUrl :: Text
    -- ^ An HTTP(S) link to the repository.
    -- This will be used to construct links to issues, commits and pull requests.
  , gitLatestVersion :: Maybe Text
    -- ^ Latest version (tag) in the current branch.
  }

-- | Get latest git tag in HEAD if present.
loadGitLatestTag :: IO (Maybe Text)
loadGitLatestTag = do
  ver <- fold ((fromRight "" <$> inprocWithErr "git" ["describe", "--tags", "--abbrev=0", "HEAD^"] empty) `catch` \ (_ :: ExitCode) -> empty) Fold.head
  return $ fmap lineToText ver

-- | Get link to origin and strip '.git' to get valid url to project page.
loadGitRemoteUrl :: IO Text
loadGitRemoteUrl = do
  raw <- strict $ inproc "git" ["remote", "get-url", "origin"] empty
  return $ fromMaybe (Text.stripEnd raw) (Text.stripSuffix ".git\n" raw)

-- | Load git history from a given commit or from the start of the project.
loadGitHistory
  :: Maybe Text  -- ^ A commit/tag to mark the start of history.
  -> IO [Turtle.Line]
loadGitHistory from = do
  fold (grep
    (invert (has (text "Merge branch"))) -- FIXME: why ignore Merge branch commits?
    (inproc "git" (["log", "--oneline", "--first-parent"] <> range) empty))
    Fold.list
  where
    range = case from of
      Nothing     -> []
      Just commit -> [commit <> "..HEAD"]

-- | Extract latest history and origin link from git through temporary file and store it in 'GitInfo'.
loadGitInfo
  :: Bool  -- ^ Include the whole project history?
  -> IO GitInfo
loadGitInfo entireHistory = do
  latestTag    <- loadGitLatestTag
  gitHistory   <- loadGitHistory (if entireHistory then Nothing else latestTag)
  gitRemoteUrl <- loadGitRemoteUrl
  let gitLatestVersion = extractVersion latestTag
  return GitInfo {..}
  where
    extractVersion tag = case Text.dropWhile (not . isDigit) <$> tag of
      Just ver | not (Text.null ver) -> Just ver
      _ -> Nothing
