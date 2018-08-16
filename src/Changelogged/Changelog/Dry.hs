{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Changelogged.Changelog.Dry where

import           Prelude              hiding (FilePath)
import           Turtle

import qualified Control.Foldl        as Fold

import           System.Console.ANSI  (Color (..))

import           Changelogged.Common
import           Changelogged.Changelog.Common

-- $setup
-- >>> :set -XOverloadedStrings

actOnMissingCommit :: FilePath -> Appl Bool -> Commit -> Appl Bool
actOnMissingCommit changelog action Commit{..} = do
  noEntry <- case commitIsPR of
    Nothing -> fold (grep (has (text (getSHA1 commitSHA))) (input changelog)) Fold.null
    Just (PR num) -> fold (grep (has (text num)) (input changelog)) Fold.null
  if noEntry
    -- If --from-bc option invoked it will prepend list of misses with version tag.
    then printCommitTag commitSHA >> action
    else return True


-- |Check if commit/pr is present in changelog. Return '@True@' if present.
listEntries :: FilePath -> [Commit] -> Appl Bool
listEntries changelog commits =
  mapM (\commit -> actOnMissingCommit changelog (warnMissing commit >> return False) commit) commits >>= return . and

-- |
warnMissing :: Commit -> Appl ()
warnMissing Commit{..} = do
  case commitIsPR of
    Nothing -> do
      printf ("- Commit ")
      coloredPrint Cyan (getSHA1 commitSHA)
    Just (PR num) -> do
      printf ("- Pull request ")
      coloredPrint Cyan (num)
  printf (" is missing: "%s%".\n") commitMessage
