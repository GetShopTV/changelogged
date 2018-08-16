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
import           Changelogged.Git     (getCommitTag)

-- $setup
-- >>> :set -XOverloadedStrings

-- |
-- >>> prLink (Link "https://github.com/GetShopTV/changelogged") (PR "#13")
-- Link {getLink = " https://github.com/GetShopTV/changelogged/pull/13 "}
prLink :: Link -> PR -> Link
prLink (Link link) (PR num) = Link $ " " <> link <> "/pull/" <> Text.drop 1 num <> " "

-- |
-- >>> commitLink (Link "https://github.com/GetShopTV/changelogged") (SHA1 "9e14840")
-- Link {getLink = " https://github.com/GetShopTV/changelogged/commit/9e14840 "}
commitLink :: Link -> SHA1 -> Link
commitLink (Link link) (SHA1 sha) = Link $ " " <> link <> "/commit/" <> sha <> " "

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
