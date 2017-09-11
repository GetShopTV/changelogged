{-# LANGUAGE DeriveGeneric #-}

module Settings where

import Data.Text (Text)
import qualified Data.Yaml as Yaml

import GHC.Generics

data Paths = Paths {
  -- Path to swagger file of API
  swaggerFileName :: Maybe Text
  } deriving (Show, Generic)

instance Yaml.FromJSON Paths

loadPaths :: IO Paths
loadPaths = do
  ms <- Yaml.decodeFileEither "./paths"
  case ms of
    Left err -> error (show err)
    Right paths -> return paths
