{-# LANGUAGE DeriveGeneric #-}
module Changelogged.Common.Types.Git where

import           Changelogged.Common.Types.Common (Link)

import           Data.Text                        (Text)
import           Turtle.Line                      (Line)

-- | Information about the state of a git repository.
data GitInfo = GitInfo
  { gitHistory       :: [Line]
    -- ^ A list of git commit messages.
  , gitRemoteUrl     :: Link
    -- ^ An HTTP(S) link to the repository.
    -- This will be used to construct links to issues, commits and pull requests.
  , gitLatestVersion :: Maybe Text
     -- ^ Latest version (tag) in the current branch. This signature avails to use git polymorphism of tags with hashes.
  } deriving (Show)
