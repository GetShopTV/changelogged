{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module Changelogged.Config where

import           Changelogged.Changelog.Config
import           Changelogged.Utils.Aeson      (deriveJSON')
import           Data.Text                     (Text)
import           GHC.Generics                  (Generic)

-- | Default project configuration.
--
-- >>> import qualified Data.ByteString.Char8 as BS8
-- >>> import qualified Data.Yaml as Yaml
-- >>> BS8.putStrLn (Yaml.encode defaultConfig)
-- changelogs:
-- - changelog: ChangeLog.md
-- <BLANKLINE>
defaultConfig :: Config
defaultConfig = Config
  { configChangelogs = [defaultChangelogConfig]
  , configBranch = Nothing
  }

-- | Changelog configuration for a project.
data Config = Config
  { configChangelogs :: [ChangelogConfig]
    -- ^ Individual changelog configurations.
  , configBranch     :: Maybe GitBranch
    -- ^ Branch with version tags to follow (if differs from current branch).
  } deriving (Eq, Show, Generic)

-- | Git branch name.
type GitBranch = Text

deriveJSON' ''Config
