-- Configuration file mapping.

{-# LANGUAGE DeriveGeneric #-}

module Settings where

import Prelude hiding (FilePath)
import Turtle

import Data.Text (Text)
import qualified Data.Yaml as Yaml

import GHC.Generics

import Types

data Paths = Paths {
  -- Changelog file name
    changeLog :: Maybe FilePath
  -- API changelog file name
  , apiChangeLog :: Maybe FilePath
  -- Path to swagger file of API
  , swaggerFileName  :: Maybe (FilePath, Variable)
  -- Default package names
  , defaultPackages  :: Maybe [Text]
  -- paths to files except for .cabal files where to bump version with corresponding variables.
  , packagesPathsWithVars :: Maybe [(FilePath, Variable)]
  -- Paths to files where API version must be bumped with names of corresponding variables.
  , apiPathsWithVars :: Maybe [(FilePath, Variable)]
  } deriving (Show, Generic)

instance Yaml.FromJSON Paths

instance Yaml.FromJSON FilePath where
  parseJSON = fmap fromText . Yaml.parseJSON

loadPaths :: IO Paths
loadPaths = do
  ms <- Yaml.decodeFileEither "./changelogged.yaml"
  case ms of
    Left errr -> error (show errr)
    Right paths -> return paths
