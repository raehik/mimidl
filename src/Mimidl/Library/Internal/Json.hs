module Mimidl.Library.Internal.Json
  ( module Mimidl.Library.Internal.Json
  , ToJSON(..), FromJSON(..)
  ) where

import Data.Aeson
import Data.Aeson.Types ( Parser )
import GHC.Generics ( Generic, Rep )

-- We cheat and use the same string for both the field label and constructor tag
-- modifier, because we figure that you'll only be using this on a product type
-- or an enum-like sum type (no inner fields).
jcMimidlLibrary :: String -> Options
jcMimidlLibrary x = defaultOptions
  { fieldLabelModifier     = labelMod
  , constructorTagModifier = labelMod
  , rejectUnknownFields    = False
  } where labelMod = camelTo2 '_' . drop (length x)

-- | Shortcut for genericParseJSON (Mimidl library)
gpjml :: (Generic a, GFromJSON Zero (Rep a)) => String -> Value -> Parser a
gpjml = genericParseJSON . jcMimidlLibrary

-- | Shortcut for genericToJSON (Mimidl library)
gtjml :: (Generic a, GToJSON' Value Zero (Rep a)) => String -> a -> Value
gtjml = genericToJSON . jcMimidlLibrary

-- | Shortcut for genericToEncoding (Mimidl library)
gteml :: (Generic a, GToJSON' Encoding Zero (Rep a)) => String -> a -> Encoding
gteml = genericToEncoding . jcMimidlLibrary
