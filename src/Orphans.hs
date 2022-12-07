{-# OPTIONS_GHC -fno-warn-orphans #-}

module Orphans where

-- req <> aeson
import Data.Aeson
import Network.HTTP.Req
import Text.URI

-- world-peace <> prettyprinter
import Data.WorldPeace
import Prettyprinter

-- TODO
instance FromJSON (Url 'Https) where
    parseJSON = withText "Url 'Https" go
      where
        go t = do
          case mkURI t of
            Left e -> fail $ show e
            Right uri ->
              case useHttpsURI uri of
                Nothing -> fail "bad"
                Just (url, _) -> pure url


instance Pretty (Union f '[]) where
  pretty = absurdUnion

instance (Pretty (f a), Pretty (Union f as)) => Pretty (Union f (a ': as)) where
  pretty = union pretty pretty
