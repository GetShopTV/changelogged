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

import Changelogged.Types
import Changelogged.Options (Appl, Options(..), asks)

-- | Information about the state of a git repository.
data GitInfo = GitInfo
  { gitHistory   :: [Turtle.Line]
    -- ^ A list of git commit messages.
  , gitRemoteUrl :: Link
    -- ^ An HTTP(S) link to the repository.
    -- This will be used to construct links to issues, commits and pull requests.
  , gitLatestVersion :: Maybe Text
     -- ^ Latest version (tag) in the current branch. This signature avails to use git polymorphism of tags with hashes.
  } deriving (Show)

-- | Get latest git tag in a given branch (if present).
-- If no branch is specified then @HEAD^@ is used.
loadGitLatestTag :: Maybe Text -> Appl (Maybe Text)
loadGitLatestTag mbranch = do
  let branch = fromMaybe "HEAD^" mbranch
  ver <- fold ((fromRight "" <$> inprocWithErr "git" ["describe", "--tags", "--abbrev=0", branch] empty) `catch` \ (_ :: ExitCode) -> empty) Fold.head
  return $ fmap lineToText ver

-- | Get link to origin and strip '.git' to get valid url to project page.
loadGitRemoteUrl :: Appl Link
loadGitRemoteUrl = Link . remoteUrlToHttps
  <$> strict (inproc "git" ["remote", "get-url", "origin"] empty)

-- | Change git remote URL so that it can be used in the browser.
--
-- >>> remoteUrlToHttps "git@github.com:GetShopTV/changelogged.git"
-- "https://github.com/GetShopTV/changelogged"
--
-- >>> remoteUrlToHttps "https://github.com/GetShopTV/changelogged.git"
-- "https://github.com/GetShopTV/changelogged"
remoteUrlToHttps :: Text -> Text
remoteUrlToHttps
  = whenPossible (Text.stripSuffix ".git")
  . Text.replace "git@github.com:" "https://github.com/"
  . Text.strip
  where
    whenPossible fn y = fromMaybe y (fn y)

-- | Load git history from a given commit or from the start of the project.
loadGitHistory
  :: Maybe Text  -- ^ A commit/tag to mark the start of history.
  -> Appl [Turtle.Line]
loadGitHistory from = fold (inproc "git" (["log", "--oneline", "--first-parent"] <> range) empty) Fold.list
  where
    range = case from of
      Nothing     -> []
      Just commit -> [commit <> "..HEAD"]

-- | Extract latest history and origin link from git through temporary file and store it in 'GitInfo'.
loadGitInfo
  :: Maybe Text -- ^ Branch with version tags (@HEAD@ is used by default).
  -> Appl GitInfo
loadGitInfo branch = do
  entireHistory <- asks optFromBC
  latestTag    <- loadGitLatestTag branch
  gitHistory   <- loadGitHistory (if entireHistory then Nothing else latestTag)
  gitRemoteUrl <- loadGitRemoteUrl
  let gitLatestVersion = extractVersion latestTag
  return GitInfo {..}
  where
    extractVersion tag = case Text.dropWhile (not . isDigit) <$> tag of
      Just ver | not (Text.null ver) -> Just ver
      _ -> Nothing

-- | Pretty print known information about a Git project.
ppGitInfo :: GitInfo -> Text
ppGitInfo GitInfo{..} = Text.unlines
  [ "Git remote URL: " <> getLink gitRemoteUrl
  , "Latest release: " <> fromMaybe "<none>" gitLatestVersion
  , "Changes since last release: " <> Text.pack (show (length gitHistory))
  ]

listPRCommits :: SHA1 -> Appl [(SHA1, Text)]
listPRCommits (SHA1 sha) = do
  messages <- fold (inproc "git" ["log", "--oneline", sha <> "^1..." <> sha <> "^2"] empty) Fold.list
  return . reverse . map ((\(first,second) -> (SHA1 first, Text.drop 1 second)) . Text.breakOn " " . lineToText) $ messages

getCommitTag :: SHA1 -> Appl (Maybe Text)
getCommitTag (SHA1 sha) = do
  tag <- fold (inproc "git" ["tag", "--points-at", sha] empty) Fold.head
  return $ lineToText <$> tag
