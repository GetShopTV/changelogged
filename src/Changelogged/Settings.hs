{-# LANGUAGE DeriveGeneric #-}

module Changelogged.Settings where

import Data.Aeson
import Prelude hiding (FilePath)
import Turtle

import qualified Control.Foldl as Fold
import Filesystem.Path.CurrentOS ((<.>), encodeString, decodeString)

import qualified Data.HashMap.Strict as HM
import Data.List (isInfixOf)
import Data.Text (Text)
import qualified Data.Yaml as Yaml

import GHC.Generics

import Changelogged.Types

data Paths = Paths {
  -- Changelogs data
    changelogs :: Maybe (HM.HashMap Text TaggedLog)
  -- Files to bump data
  , versioned :: Maybe (HM.HashMap Text [TaggedFile])
  } deriving (Show, Generic)

instance FromJSON Paths

makeDefaultPaths :: IO Paths
makeDefaultPaths = do
  cabals <- fold (find (suffix (text "package.yaml")) ".") Fold.list
  let textualCabals = map encodeString cabals
      filedCabals = map decodeString
      taggedFiles = map (`TaggedFile` "version") (filedCabals $ filter (not . isInfixOf "/.") textualCabals)
      defaultChLog = TaggedLog ("ChangeLog" <.> "md") Nothing
  return $ Paths (Just $ HM.singleton "main" defaultChLog) (Just $ HM.singleton "main" taggedFiles)

loadPaths :: IO (Maybe Paths)
loadPaths = do
  ms <- Yaml.decodeFileEither "./changelogged.yaml"
  return $ case ms of
    Left _wrong -> Nothing
    Right paths -> Just paths
