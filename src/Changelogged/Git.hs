-- | This module is intended to be pure git interface.
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Changelogged.Git where

import qualified System.Process as Proc

import qualified Control.Foldl           as Fold
import           Control.Monad           (void)
import           Control.Monad.Catch     (catch)

import           Data.Either.Combinators (fromRight)
import           Data.Maybe              (fromMaybe)
import           Data.String.Conversions
import           Data.Text               (Text)
import qualified Data.Text               as Text

import           Turtle

import           Changelogged.Pattern
import           Changelogged.Common

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
loadGitHistory from = reverse <$> fold (inproc "git" (["log", "--oneline", "--first-parent"] <> range) empty) Fold.list
  where
    range = case from of
      Nothing     -> []
      Just commit -> [commit <> "..HEAD"]

-- |Get commit message for any entry in history.
retrieveCommitMessage :: Maybe PR -> SHA1 -> Appl Text
retrieveCommitMessage isPR (SHA1 commit) = do
  summary <- fold (inproc "git" ["show", "-s", "--format=%B", commit] empty) Fold.list
  return $ Text.stripStart $ lineToText $ case isPR of
    Just _  -> summary !! 2
    Nothing -> summary !! 0

messageToCommitData :: Line -> Appl Commit
messageToCommitData message = do
  --FIXME: departed proofs?
  commitIsPR <- fmap (PR . fromJustCustom "Cannot find commit hash in git log entry" . githubRefMatch . lineToText) <$>
    fold (grep githubRefGrep (select [message])) Fold.head
  let commitSHA = SHA1 . fst . Text.breakOn " " . lineToText $ message
  commitMessage <- retrieveCommitMessage commitIsPR commitSHA
  return Commit{..}

listPRCommits :: SHA1 -> Appl [Commit]
listPRCommits (SHA1 sha) = do
  messages <- fold (inproc "git" ["log", "--oneline", sha <> "^1..." <> sha <> "^2"] empty) Fold.list
  commits <- mapM messageToCommitData messages
  return . reverse $ commits

getCommitTag :: SHA1 -> Appl (Maybe Text)
getCommitTag (SHA1 sha) = do
  tag <- fold (inproc "git" ["tag", "--points-at", sha] empty) Fold.head
  return $ lineToText <$> tag

showDiff :: SHA1 -> Appl ()
showDiff (SHA1 sha) = do
  args <- buildArgs
  (lessHandle, gitHandle) <- liftIO Proc.createPipe
  -- FIXME: make Process template in Utils.
  (_,_,_,lessWaiter) <- liftIO $ Proc.createProcess templateProcess
    -- FIXME: make pager configurable. Now it's breaking Windows support.
    { Proc.cmdspec = (Proc.RawCommand "less" ["-r"])
    , Proc.std_in = (Proc.UseHandle lessHandle)
    }
  void . liftIO $ Proc.runProcess "git" args Nothing Nothing Nothing (Just gitHandle) Nothing
  void . liftIO . Proc.waitForProcess $ lessWaiter
  
  --stdout $ inproc "git" args empty
  where
    buildArgs = do
      noColor <- gets (optNoColors . envOptions)
      return . map cs $ if noColor
        then ["show", "--minimal", "--color=never", sha]
        else ["show", "--minimal", "--color=always", sha]
