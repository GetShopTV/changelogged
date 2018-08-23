{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Changelogged.Changelog.Common where

import           Prelude              hiding (FilePath)
import           Turtle

import qualified Control.Foldl        as Fold

import           Data.Foldable        (asum)
import qualified Data.Text            as Text

import           System.Console.ANSI  (Color (..))

import           Changelogged.Common
import           Changelogged.Git     (getCommitTag, parseHostingType)

-- $setup
-- >>> :set -XOverloadedStrings

prLink :: Link -> PR -> Link
prLink link pr =
  let hostingType = parseHostingType link in
  case hostingType of
    GitHub -> githubPrLink link pr
    -- FIXME: add builders for Bitbucket and Gitlab links.
    BitBucket -> githubPrLink link pr
    GitLab -> githubPrLink link pr

-- |
-- >>> githubPrLink (Link "https://github.com/GetShopTV/changelogged") (PR "#13")
-- Link {getLink = " https://github.com/GetShopTV/changelogged/pull/13 "}
githubPrLink :: Link -> PR -> Link
githubPrLink (Link link) (PR num) = Link $ " " <> link <> "/pull/" <> Text.drop 1 num <> " "

commitLink :: Link -> SHA1 -> Link
commitLink link hash =
  let hostingType = parseHostingType link in
  case hostingType of
    GitHub -> githubCommitLink link hash
    -- FIXME: add builders for Bitbucket and Gitlab links.
    BitBucket -> githubCommitLink link hash
    GitLab -> githubCommitLink link hash

-- |
-- >>> githubCommitLink (Link "https://github.com/GetShopTV/changelogged") (SHA1 "9e14840")
-- Link {getLink = " https://github.com/GetShopTV/changelogged/commit/9e14840 "}
githubCommitLink :: Link -> SHA1 -> Link
githubCommitLink (Link link) (SHA1 sha) = Link $ " " <> link <> "/commit/" <> sha <> " "

printCommitTag :: SHA1 -> Appl ()
printCommitTag sha = getCommitTag sha >>= \tag -> case tag of
  Nothing -> return ()
  Just t  -> coloredPrint Yellow (t <> "\n")

allFilesIgnored :: Maybe [FilePath] -> SHA1 -> Appl Bool
allFilesIgnored Nothing _ = return False
allFilesIgnored (Just files) (SHA1 commit) = do
  affectedFiles <- fold (inproc "git" ["diff-tree", "--name-only", "--no-commit-id", "-m", "-r", commit] empty) Fold.list
  let expandIgnoredFiles = map makeWildcardPattern (map encodeString files)
  return . null . filter null . map (match (choice expandIgnoredFiles)) $ (map lineToText affectedFiles)

commitNotWatched :: Maybe [FilePath] -> SHA1 -> Appl Bool
commitNotWatched Nothing _ = return False
commitNotWatched (Just files) (SHA1 commit) = let expandWatchFiles = asum (map makeWildcardPattern (map encodeString files)) in
  fold
      (grep expandWatchFiles
      (inproc "git" ["diff-tree", "--name-only", "--no-commit-id", "-m", "-r", commit] empty))
    Fold.null

commitIgnored :: Maybe [SHA1] -> SHA1 -> Appl Bool
commitIgnored Nothing _ = return False
commitIgnored (Just commits) commit = return $ commit `elem` commits
