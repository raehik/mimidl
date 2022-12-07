module HGCloud.Util.Json where

import Data.Aeson
import Data.Aeson.Types
import GHC.Generics

-- | Shortcut for genericParseJSON (GCloud)
gpjgc :: (Generic a, GFromJSON Zero (Rep a)) => String -> Value -> Parser a
gpjgc = genericParseJSON . jcGCloud

-- We cheat and use the same string for both the field label and constructor tag
-- modifier, because we figure that you'll only be using this on a product type
-- or an enum-like sum type (no inner fields).
jcGCloud :: String -> Options
jcGCloud x = defaultOptions
  { fieldLabelModifier = camelTo2 '_' . drop (length x)
  , constructorTagModifier = camelTo2 '_' . drop (length x)
  , rejectUnknownFields = False }
