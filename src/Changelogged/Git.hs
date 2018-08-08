-- | This module is intended to be pure git interface.
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Changelogged.Git where

import qualified Control.Foldl as Fold
import Control.Monad.Catch (catch)

import Data.Either.Combinators (fromRight)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

import Turtle

import Changelogged.Common

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

listPRCommits :: SHA1 -> Appl [(SHA1, Text)]
listPRCommits (SHA1 sha) = do
  messages <- fold (inproc "git" ["log", "--oneline", sha <> "^1..." <> sha <> "^2"] empty) Fold.list
  return . reverse . map ((\(first,second) -> (SHA1 first, Text.drop 1 second)) . Text.breakOn " " . lineToText) $ messages

getCommitTag :: SHA1 -> Appl (Maybe Text)
getCommitTag (SHA1 sha) = do
  tag <- fold (inproc "git" ["tag", "--points-at", sha] empty) Fold.head
  return $ lineToText <$> tag
