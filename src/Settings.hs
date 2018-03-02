{-# LANGUAGE DeriveGeneric #-}

module Settings where

import Prelude hiding (FilePath)
import Turtle

import qualified Control.Foldl as Fold
import Filesystem.Path.CurrentOS ((<.>), encodeString, decodeString)

import Data.Aeson.Types (typeMismatch)
import qualified Data.HashMap.Strict as HM
import Data.List (isInfixOf)
import Data.Text (Text)
import qualified Data.Yaml as Yaml
import Data.Vector ((!))
import Data.Yaml ((.:), (.:?))

import GHC.Generics

import Types

data Paths = Paths {
  -- Changelogs data
    changelogs :: Maybe (HM.HashMap Text TaggedLog)
  -- Files to bump data
  , versioned :: Maybe (HM.HashMap Text [TaggedFile])
  } deriving (Show, Generic)

makeDefaultPaths :: IO Paths
makeDefaultPaths = do
  cabals <- fold (find (suffix (text "package.yaml")) ".") Fold.list
  let textualCabals = map encodeString cabals
      filedCabals a = map decodeString a
      taggedFiles = map (\path -> TaggedFile path "version") (filedCabals $ filter (not . isInfixOf "/.") textualCabals)
      defaultChLog = TaggedLog ("ChangeLog" <.> "md") Nothing
  return $ Paths (Just $ HM.singleton "main" defaultChLog) (Just $ HM.singleton "main" taggedFiles)

instance Yaml.FromJSON Paths

instance Yaml.FromJSON TaggedFile where
  parseJSON (Yaml.Object v) = TaggedFile
        <$> v .: "path"
        <*> v .: "variable"
  parseJSON (Yaml.Array v) = TaggedFile
        <$> Yaml.parseJSON (v ! 0)
        <*> Yaml.parseJSON (v ! 1)
  parseJSON invalid = typeMismatch "TaggedFile" invalid

instance Yaml.FromJSON TaggedLog where
  parseJSON (Yaml.Object v) = TaggedLog
        <$> v .: "path"
        <*> v .:? "indicator"
  parseJSON (Yaml.Array v) = TaggedLog
        <$> Yaml.parseJSON (v ! 0)
        <*> Yaml.parseJSON (v ! 1)
  parseJSON invalid = typeMismatch "TaggedLog" invalid

instance Yaml.FromJSON FilePath where
  parseJSON = fmap fromText . Yaml.parseJSON

loadPaths :: IO (Maybe Paths)
loadPaths = do
  ms <- Yaml.decodeFileEither "./changelogged.yaml"
  return $ case ms of
    Left _wrong -> Nothing
    Right paths -> Just paths
