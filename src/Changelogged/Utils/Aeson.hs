{-# LANGUAGE ScopedTypeVariables #-}
module Changelogged.Utils.Aeson where

import           Changelogged.Utils.Naming (nameModifier)
import           Data.Aeson
import           Data.Aeson.TH             (deriveJSON)
import           Language.Haskell.TH       (Dec, Name, Q, nameBase)

-- | Options to derive @'ToJSON'@/@'FromJSON'@ instances.
--
-- @aesonOptions prefix@ drops @prefix@ for every field and converts
-- what's left to @lowerCamelCase@.
aesonOptions :: String -> Options
aesonOptions prefix = defaultOptions
  { fieldLabelModifier      = nameModifier prefix
  , constructorTagModifier  = nameModifier prefix
  , sumEncoding             = ObjectWithSingleField
  , omitNothingFields       = True
  }

-- | Derive 'ToJSON' and 'FromJSON' with Template Haskell using 'aesonOptions'.
deriveJSON' :: Name -> Q [Dec]
deriveJSON' typeName = deriveJSON (aesonOptions (nameBase typeName)) typeName
