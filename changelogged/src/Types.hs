{-# LANGUAGE DeriveGeneric #-}
module Types where

import Data.Text (Text)

import Prelude hiding (FilePath)
import Turtle hiding (option)

import GHC.Generics

type Variable = Text
type Key = Text

-- |Level of changes to bump to.
data Level = App | Major | Minor | Fix | Doc
  deriving (Show, Enum, Bounded)

-- |Type of entry in git history.
data Mode = PR | Commit

-- |Structure to save once token git data.
data Git = Git
  { gitHistory :: FilePath
  , gitLink :: Text
  , gitRevision :: Text
  }

-- |File with identifier of version to bump.
data TaggedFile = TaggedFile
  { taggedFilePath :: FilePath
  , taggedFileVariable :: Variable
  } deriving (Show, Generic)

-- |Changelog with optional file indicating changes.
data TaggedLog = TaggedLog
  { taggedLogPath :: FilePath
  , taggedLogIndicator :: Maybe TaggedFile
  } deriving (Show, Generic)

instance Show Mode where
  show PR = "Pull request"
  show Commit = "Single commit"

data WarningFormat
  = WarnSimple
  | WarnSuggest
  deriving (Enum, Bounded)

instance Show WarningFormat where
  show WarnSimple  = "simple"
  show WarnSuggest = "suggest"

data Options = Options
  { -- |Explicit level of changes for files with common versioning.
    optPackagesLevel   :: Maybe Level
  -- |Explicit level of changes in API.
  , optApiLevel        :: Maybe Level
  -- |Output formatting.
  , optFormat          :: WarningFormat
  -- |Assume there is API to check and bump.
  , optWithAPI         :: Bool
  -- |Assume there is some changelogs with unpredicted meanings.
  , optDifferentChlogs :: Bool
  -- |Do not check changelogs.
  , optNoCheck         :: Bool
  -- |Do not bump versions.
  , optNoBump          :: Bool
  -- |Check changelogs from start of project.
  , optFromBC          :: Bool
  -- |Bump versions even if changelogs are outdated.
  , optForce           :: Bool
  }
