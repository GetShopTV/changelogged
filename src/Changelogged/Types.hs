{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Changelogged.Types where

import Data.Aeson (ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

type Variable = Text
type Key = Text

-- |Level of changes to bump to.
data Level = App | Major | Minor | Fix | Doc
  deriving (Generic, Show, Enum, Bounded, ToJSON)

-- |Type of entry in git history.
data Mode = PR | Commit

instance Show Mode where
  show PR = "Pull request"
  show Commit = "Commit"

data WarningFormat
  = WarnSimple
  | WarnSuggest
  deriving (Generic, Eq, Enum, Bounded, ToJSON)

instance Show WarningFormat where
  show WarnSimple  = "simple"
  show WarnSuggest = "suggest"
