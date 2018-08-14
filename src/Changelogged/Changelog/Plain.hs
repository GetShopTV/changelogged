{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Changelogged.Changelog.Plain where

import           Prelude              hiding (FilePath)
import           Turtle

import           System.Console.ANSI  (Color (..))

import           Changelogged.Common
import           Changelogged.Changelog.Common

-- $setup
-- >>> :set -XOverloadedStrings

-- |Check if commit/pr is present in changelog. Return '@True@' if present.
plainDealWithEntry :: Commit -> FilePath -> Appl Bool
plainDealWithEntry commit@Commit{..} changelog =
  actOnMissingCommit commit changelog $ do
    warnMissing commit
    return False

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
