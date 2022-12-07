-- | Mimicle API implementation via the live @mimicle.com@ service.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Mimidl.Mimicle.Api.Impl.MimicleCom
  ( module Mimidl.Mimicle.Api.Impl.MimicleCom
  , module Mimidl.Mimicle.Api.Impl.MimicleCom.Authd
  , module Mimidl.Mimicle.Api.Impl.MimicleCom.Anon
  , module Mimidl.Mimicle.Api.Impl.MimicleCom.Common
  ) where

import Mimidl.Mimicle.Api.Model
import Mimidl.Mimicle.Types
import Mimidl.Mimicle.Api.Impl.MimicleCom.Authd
import Mimidl.Mimicle.Api.Impl.MimicleCom.Anon
import Mimidl.Mimicle.Api.Impl.MimicleCom.Common
import Mimidl.Mimicle.Api.Impl.Log

import HGCloud.Firebase.Authentication qualified as GC.FB.Auth
import HGCloud.Firebase.Authentication ( IdToken )

import Polysemy
import Polysemy.State
import Polysemy.Error
import Polysemy.Output

import Data.WorldPeace

import Data.Text ( Text )
import Data.Text qualified as Text

import Data.ByteString qualified as B

import Network.HTTP.Req
import Text.URI ( mkURI )

import Control.Monad.IO.Class

import Data.Foldable ( foldl' )

-- with itok caching via state (and backoff via error)
runMimicle
    :: forall es r a
    .  ( Members '[ GC.FB.Auth.Api
                  , State (Maybe IdToken)
                  , Error (OpenUnion es)
                  , Embed IO
                  , Output Log
                  ] r
       , Contains '[EAuthBackoff] es )
    => Url 'Https -> AuthToken -> Sem (Api ': r) a -> Sem r a
runMimicle base atok = interpret $ \case
  ApiAuthd action -> runMimicleAuthd' @es base atok action
  ApiAnon  action -> runMimicleAnon'      base      action

--------------------------------------------------------------------------------

-- | Download a subpath inside a permission's content filepath.
downloadPermissionContent
    :: MonadIO m
    => (Permission -> Option 'Https)
    -> Permission -> [Text] -> m B.ByteString
downloadPermissionContent f p subpaths = runReq defaultHttpConfig $ do
    contentPathUri <- mkURI $ Text.init $ permissionContentPath p -- TODO ??
    let Just (contentPath, _) = useHttpsURI contentPathUri -- TODO assuming safe
        url = foldl' (/:) contentPath subpaths
    r <- req GET url NoReqBody bsResponse $ f p
    pure $ responseBody r

searchAlbumViaNid :: Nid -> [Album] -> Maybe Album
searchAlbumViaNid nid as =
    case as' of
      []  -> Nothing
      [a] -> Just a
      _   -> error "non-unique nids present in response (likely API or site bug)"
  where
    as' = filter go as
    go a = if albumNid a == nid then True else False
