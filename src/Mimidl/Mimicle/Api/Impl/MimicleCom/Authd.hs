-- | Mimicle authenticated API implementation via the live @mimicle.com@
--   service.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Mimidl.Mimicle.Api.Impl.MimicleCom.Authd where

import Mimidl.Mimicle.Api.Model
import Mimidl.Mimicle.Types
import Mimidl.Mimicle.Api.Impl.MimicleCom.Common
import Mimidl.Mimicle.Api.Impl.Log

import HGCloud.Firebase.Authentication qualified as GC.FB.Auth
import HGCloud.Firebase.Authentication ( IdToken )

import Polysemy
import Polysemy.State
import Polysemy.Error
import Polysemy.Output

import Data.WorldPeace

import Network.HTTP.Req

import Numeric.Natural ( Natural )

import Prettyprinter qualified as Pretty
import Prettyprinter ( Pretty(..) )

import GHC.Generics ( Generic )

data AuthToken
  = AuthTokenRefresh GC.FB.Auth.RefreshToken
  | AuthTokenId      GC.FB.Auth.IdToken
    deriving stock (Generic, Show, Eq)

-- | Interpret authenticated Mimicle API requests using the live @mimicle.com@
--   service.
runMimicleAuthd'
    :: forall es rInitial r a
    .  ( Members '[ GC.FB.Auth.Api
                  , State (Maybe IdToken)
                  , Error (OpenUnion es)
                  , Embed IO
                  , Output Log
                  ] r
       , Contains '[EAuthBackoff] es )
    => Url 'Https -> AuthToken -> ApiAuthd (Sem rInitial) a -> Sem r a
runMimicleAuthd' base atok = \case

  PermissionGenerate uid -> wrapAuth @es atok $ \itok -> do
    let url = base /: "permission"/:"generate"/:getId uid
        itokBytes = GC.FB.Auth.idTokenBytes itok
    r <- requestIdWithLog "resource permission" uid Nothing $
        runReq defaultHttpConfig $
            req GET url NoReqBody jsonResponse (oAuth2Bearer itokBytes)
    pure $ responseBody r

  MyPurchasedAlbums -> wrapAuth @es atok $ \itok -> do
    let url = base /: "my"/:"purchased"/:"albums"
        itokBytes = GC.FB.Auth.idTokenBytes itok
    reqPaginatedAll "list of purchased albums" 100 defaultHttpConfig url (oAuth2Bearer itokBytes)

--------------------------------------------------------------------------------

-- | Error: Tried to authenticate a few times and failed.
data EAuthBackoff = EAuthBackoff
    deriving stock (Generic, Show, Eq)
instance Pretty EAuthBackoff where pretty = Pretty.unsafeViaShow

wrapAuth
    :: forall es r a
    .  ( Members '[ GC.FB.Auth.Api
                  , State (Maybe IdToken)
                  , Error (OpenUnion es)
                  , Output Log
                  ] r
       , Contains '[EAuthBackoff] es )
    => AuthToken -> (IdToken -> Sem r a) -> Sem r a
wrapAuth atok f = obtainIdToken @es 1 atok >>= f

-- | ID token handling with caching and backoff.
--
-- ...The backoff doesn't actually get used right now, because the Mimicle API
-- model doesn't handle API failures. That needs to be added, then requests
-- wrapped with a retrier.
obtainIdToken
    :: forall es r
    .  ( Members '[ GC.FB.Auth.Api
                  , State (Maybe IdToken)
                  , Error (OpenUnion es)
                  , Output Log
                  ] r
       , Contains '[EAuthBackoff] es )
    => Natural -> AuthToken -> Sem r IdToken
obtainIdToken backoff atok =
    get >>= \case
      Just itok -> pure itok
      Nothing   ->
        if backoff == 0 then
            throwOU @es EAuthBackoff
        else
            case atok of
              AuthTokenId itok -> do
                put $ Just itok
                pure itok
              AuthTokenRefresh rtok -> do
                itok <- requestWithLog "user authentication" Nothing $
                    GC.FB.Auth.token rtok
                put $ Just itok
                pure itok
