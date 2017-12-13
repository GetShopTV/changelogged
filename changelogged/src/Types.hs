-- Types and helpers for choo-choo util.

module Types where

import Data.Text (Text)

import Prelude hiding (FilePath)
import Turtle hiding (option)

type Variable = Text

-- |This is not actaully used except for pretty print.
data Part = API | Project

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
  { optPackages      :: Maybe [Text]
  , optPackagesLevel :: Maybe Level
  , optApiLevel      :: Maybe Level
  , optFormat        :: WarningFormat
  , optApiExists     :: Bool
  , optNoCheck       :: Bool
  , optNoBump        :: Bool
  , optFromBC        :: Bool
  , optForce         :: Bool
  }
