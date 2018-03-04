{-# LANGUAGE DeriveGeneric #-}
module Changelogged.Types where

import Data.Text (Text)

type Variable = Text
type Key = Text

-- |Level of changes to bump to.
data Level = App | Major | Minor | Fix | Doc
  deriving (Show, Enum, Bounded)

-- |Type of entry in git history.
data Mode = PR | Commit

instance Show Mode where
  show PR = "Pull request"
  show Commit = "Single commit"

data WarningFormat
  = WarnSimple
  | WarnSuggest
  deriving (Eq, Enum, Bounded)

instance Show WarningFormat where
  show WarnSimple  = "simple"
  show WarnSuggest = "suggest"
