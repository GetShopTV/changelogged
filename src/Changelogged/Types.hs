{-# LANGUAGE DeriveGeneric #-}
module Changelogged.Types where

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Text (Text)
import Data.Vector ((!))

import Prelude hiding (FilePath)
import Turtle

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

instance FromJSON TaggedFile where
  parseJSON (Object v) = TaggedFile
        <$> fmap fromText (v .: "path")
        <*> v .: "variable"
  parseJSON (Array v) = TaggedFile
        <$> fmap fromText (parseJSON (v ! 0))
        <*> parseJSON (v ! 1)
  parseJSON invalid = typeMismatch "TaggedFile" invalid

instance FromJSON TaggedLog where
  parseJSON (Object v) = TaggedLog
        <$> fmap fromText (v .: "path")
        <*> v .:? "indicator"
  parseJSON (Array v) = TaggedLog
        <$> fmap fromText (parseJSON (v ! 0))
        <*> parseJSON (v ! 1)
  parseJSON invalid = typeMismatch "TaggedLog" invalid
