-- | Partial model of Mimicle's internal API.

{-# LANGUAGE TemplateHaskell #-}

module Mimidl.Mimicle.Api.Model where

import Mimidl.Mimicle.Types

import Polysemy

-- | Anonymous requests in Mimicle internal REST API.
--
data ApiAnon m a where
    AlbumLatest :: TargetGender -> ApiAnon m [SearchAlbum]
    -- ^ @GET /album/latest@

    Album' :: Id -> ApiAnon m Album
    -- ^ @GET /album/[ID]@

deriving stock instance Show (ApiAnon m a)

makeSem ''ApiAnon

-- | Authenticated requests in Mimicle internal REST API.
--
data ApiAuthd m a where
    PermissionGenerate :: Id -> ApiAuthd m Permission
    -- ^ @GET /permission/generate/[ID]@
    --
    -- Request a permission token for the given ID (some unique content
    -- identifier).

    MyPurchasedAlbums :: ApiAuthd m [Album]
    -- ^ @GET /my/purchased/albums@

makeSem ''ApiAuthd

deriving stock instance Show (ApiAuthd m a)

-- | Mimicle internal REST API.
--
-- For better granularity, requests are split into ones which require user
-- authentication, and ones which do not (anonymous).
data Api m a where
    ApiAuthd :: ApiAuthd m a -> Api m a
    ApiAnon  :: ApiAnon  m a -> Api m a

makeSem ''Api

deriving stock instance Show (Api m a)
