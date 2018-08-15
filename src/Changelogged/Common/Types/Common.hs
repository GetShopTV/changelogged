{-# LANGUAGE DeriveAnyClass              #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE DerivingStrategies          #-}
module Changelogged.Common.Types.Common where

import           Data.Aeson
import           Data.Text    (Text)
import           GHC.Generics (Generic)

newtype SHA1 = SHA1 {getSHA1 :: Text} deriving (Eq, Show)
newtype Link = Link {getLink :: Text} deriving (Eq, Show)
newtype PR = PR {getPR :: Text} deriving (Eq, Show)
newtype EntryFormat = EntryFormat {getEntryFormat :: Text} deriving (Eq, Show, Generic)
                                                           deriving newtype Monoid
newtype Version = Version {getVersion :: Text} deriving (Eq, Show)

data Commit = Commit
  { commitMessage :: Text
  , commitIsPR    :: Maybe PR
  , commitSHA     :: SHA1
  } deriving (Eq, Show)

-- |Level of changes to bump to.
data Level = Application | Major | Minor | Fix | Doc
  deriving (Generic, Show, Enum, Bounded, ToJSON)

-- |Available altenative actions
data Action = BumpVersions
  deriving (Generic, Eq, Show, Enum, Bounded, ToJSON)

data Interaction = Write | Expand | Skip | Remind | IgnoreAlways
  deriving (Generic, Eq, Show, Enum, Bounded, ToJSON)
