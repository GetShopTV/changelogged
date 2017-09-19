{-# LANGUAGE DeriveGeneric #-}

module Settings where

import Data.Text (Text)
import qualified Data.Yaml as Yaml

import GHC.Generics

data Paths = Paths {
  -- Changelog file name
    changeLog :: Maybe Text
  -- API changelog file name
  , apiChangeLog :: Maybe Text
  -- Path to swagger file of API
  , swaggerFileName  :: Maybe (Text, Text)
  -- Default package names
  , defaultPackages  :: Maybe [Text]
  -- paths to files except for .cabal files where to bump version with corresponding variables.
  , packagesPathsWithVars :: Maybe [(Text, Text)]
  -- Paths to files where API version must be bumped with names of corresponding variables.
  , apiPathsWithVars :: Maybe [(Text, Text)]
  } deriving (Show, Generic)

instance Yaml.FromJSON Paths

loadPaths :: IO Paths
loadPaths = do
  ms <- Yaml.decodeFileEither "./paths"
  case ms of
    Left err -> error (show err)
    Right paths -> return paths
