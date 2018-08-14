{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Changelogged.Changelog.Common where

import           Prelude              hiding (FilePath)
import           Turtle

import qualified Control.Foldl        as Fold

import           Data.Foldable        (asum)
import           Data.Text            (Text)
import qualified Data.Text            as Text

import           System.Console.ANSI  (Color (..))

import           Changelogged.Common
import           Changelogged.Git     (getCommitTag)

-- $setup
-- >>> :set -XOverloadedStrings

actOnMissingCommit :: Commit -> FilePath -> Appl Bool -> Appl Bool
actOnMissingCommit Commit{..} changelog action = do
  noEntry <- case commitIsPR of
    Nothing -> fold (grep (has (text (getSHA1 commitSHA))) (input changelog)) Fold.null
    Just (PR num) -> fold (grep (has (text num)) (input changelog)) Fold.null
  if noEntry
    -- If --from-bc option invoked it will prepend list of misses with version tag.
    then printCommitTag commitSHA >> action
    else return True

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

-- |Get commit message for any entry in history.
retrieveCommitMessage :: Maybe PR -> SHA1 -> Appl Text
retrieveCommitMessage isPR (SHA1 commit) = do
  summary <- fold (inproc "git" ["show", "-s", "--format=%B", commit] empty) Fold.list
  return $ Text.stripStart $ lineToText $ case isPR of
    Just _  -> summary !! 2
    Nothing -> summary !! 0

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

commitIgnored :: Maybe [Text] -> SHA1 -> Appl Bool
commitIgnored Nothing _ = return False
commitIgnored (Just names) (SHA1 commit) = not <$> fold
  (grep (asum (map text names))
    (inproc "git" ["show", "-s", "--format=%B", commit] empty))
  Fold.null
