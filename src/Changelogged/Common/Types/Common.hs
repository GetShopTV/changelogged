{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Changelogged.Common.Types.Common where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

newtype SHA1 = SHA1 {getSHA1 :: Text} deriving (Eq, Show)
newtype Link = Link {getLink :: Text} deriving (Eq, Show)
newtype PR = PR {getPR :: Text} deriving (Eq, Show)
newtype Version = Version {getVersion :: Text} deriving (Eq, Show)

data Commit = Commit
  { commitMessage :: Text
  , commitIsPR    :: Maybe PR
  , commitSHA     :: SHA1
  } deriving (Eq, Show)

-- |Level of changes to bump to.
data Level = App | Major | Minor | Fix | Doc
  deriving (Generic, Show, Enum, Bounded, ToJSON)

-- |Available altenative actions
data Action = UpdateChangelogs | BumpVersions
  deriving (Generic, Eq, Show, Enum, Bounded, ToJSON)

data WarningFormat
  = WarnSimple
  | WarnSuggest
  deriving (Generic, Eq, Enum, Bounded, ToJSON)

instance Show WarningFormat where
  show WarnSimple  = "simple"
  show WarnSuggest = "suggest"