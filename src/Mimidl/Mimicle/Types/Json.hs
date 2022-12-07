-- | Helper definitions for defining JSON instances for Mimicle API data types.

module Mimidl.Mimicle.Types.Json where

import Data.Aeson
import Data.Aeson.Types ( Parser )
import GHC.Generics ( Generic, Rep )

import Data.Char qualified

decaps :: String -> String
decaps = \case
  ""   -> ""
  c:cs -> Data.Char.toLower c : cs

-- We cheat and use the same string for both the field label and constructor tag
-- modifier, because we figure that you'll only be using this on a product type
-- or an enum-like sum type (no inner fields).
jcMimicle :: String -> Options
jcMimicle x = defaultOptions
  { fieldLabelModifier = decaps . drop (length x)
  , constructorTagModifier = decaps . drop (length x)
  , rejectUnknownFields = False }

-- | Shortcut for genericParseJSON (Mimicle)
gpjm :: (Generic a, GFromJSON Zero (Rep a)) => String -> Value -> Parser a
gpjm = genericParseJSON . jcMimicle

-- | Shortcut for genericToJSON (Mimicle)
gtjm :: (Generic a, GToJSON' Value Zero (Rep a)) => String -> a -> Value
gtjm = genericToJSON . jcMimicle

-- | Shortcut for genericToEncoding (Mimicle)
gtem :: (Generic a, GToJSON' Encoding Zero (Rep a)) => String -> a -> Encoding
gtem = genericToEncoding . jcMimicle
