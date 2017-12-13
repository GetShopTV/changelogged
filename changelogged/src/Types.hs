-- Types and helpers for choo-choo util.

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

data TaggedFile = TaggedFile
  { taggedFilePath :: FilePath
  , taggedFileVariable :: Variable
  } deriving (Show, Generic)

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
  { optPackagesLevel   :: Maybe Level
  , optApiLevel        :: Maybe Level
  , optFormat          :: WarningFormat
  , optWithAPI         :: Bool
  , optDifferentChlogs :: Bool
  , optNoCheck         :: Bool
  , optNoBump          :: Bool
  , optFromBC          :: Bool
  , optForce           :: Bool
  }
