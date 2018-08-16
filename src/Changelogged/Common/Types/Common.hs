{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE DeriveAnyClass              #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE DerivingStrategies          #-}
module Changelogged.Common.Types.Common where

import           Data.Aeson
import           Data.Text    (Text)
import           GHC.Generics (Generic)

newtype SHA1 = SHA1 {getSHA1 :: Text} deriving (Eq, Show)
                                      deriving  newtype (ToJSON, FromJSON)
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
data Level = App | Major | Minor | Fix | Doc
  deriving (Generic, Show, Enum, Bounded, ToJSON)

showHumanReadableLevel :: Level -> Text
showHumanReadableLevel App = "application level changes"
showHumanReadableLevel Major = "major changes"
showHumanReadableLevel Minor = "minor changes"
showHumanReadableLevel Fix = "fixes"
showHumanReadableLevel Doc = "documentation changes"

data Interaction = Write | Expand | Skip | Remind | IgnoreAlways | Quit | WriteRest
  deriving (Generic, Eq, Show, Enum, Bounded, ToJSON)
