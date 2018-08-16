{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
module Changelogged.Aeson () where

import           Data.Aeson                     (FromJSON (..), ToJSON (..))
import           Data.Aeson.TH                  (deriveJSON)

import qualified Filesystem.Path.CurrentOS      as Path

import           Changelogged.Common.Types
import           Changelogged.Common.Utils.Pure

instance FromJSON Path.FilePath where
  parseJSON = fmap Path.decodeString . parseJSON

instance ToJSON Path.FilePath where
  toJSON = toJSON . Path.encodeString

instance FromJSON EntryFormat where
  parseJSON = fmap EntryFormat . parseJSON

instance ToJSON EntryFormat where
  toJSON = toJSON . getEntryFormat

deriving instance ToJSON Options

deriveJSON (jsonDerivingModifier "VersionPattern") ''VersionPattern
deriveJSON (jsonDerivingModifier "VersionFile") ''VersionFile
deriveJSON (jsonDerivingModifier "Changelog") ''ChangelogConfig
deriveJSON (jsonDerivingModifier "Config") ''Config
