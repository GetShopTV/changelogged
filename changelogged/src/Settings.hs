{-# LANGUAGE DeriveGeneric #-}

module Settings where

import Prelude hiding (FilePath)
import Turtle

import Data.Aeson.Types (typeMismatch)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Yaml as Yaml
import Data.Yaml ((.:), (.:?))

import GHC.Generics

import Types

data Paths = Paths {
  -- Changelogs data
    changelogs :: Maybe (HM.HashMap Text TaggedLog)
  -- Files to bump data
  , versioned :: Maybe (HM.HashMap Text [TaggedFile])
  } deriving (Show, Generic)

instance Yaml.FromJSON Paths

instance Yaml.FromJSON TaggedFile where
  parseJSON (Yaml.Object v) = TaggedFile
        <$> v .: "path"
        <*> v .: "variable"
  parseJSON invalid = typeMismatch "TaggedFile" invalid

instance Yaml.FromJSON TaggedLog where
  parseJSON (Yaml.Object v) = TaggedLog
        <$> v .: "path"
        <*> v .:? "indicator"
  parseJSON invalid = typeMismatch "TaggedLog" invalid

instance Yaml.FromJSON FilePath where
  parseJSON = fmap fromText . Yaml.parseJSON

loadPaths :: IO Paths
loadPaths = do
  ms <- Yaml.decodeFileEither "./changelogged.yaml"
  case ms of
    Left wrong -> error (show wrong)
    Right paths -> return paths
