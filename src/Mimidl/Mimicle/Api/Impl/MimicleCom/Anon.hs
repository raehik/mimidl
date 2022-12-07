-- | Mimicle anonymous API implementation via the live @mimicle.com@ service.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Mimidl.Mimicle.Api.Impl.MimicleCom.Anon where

import Mimidl.Mimicle.Api.Model
import Mimidl.Mimicle.Types
import Mimidl.Mimicle.Api.Impl.MimicleCom.Common
import Mimidl.Mimicle.Api.Impl.Log

import Polysemy
import Polysemy.Output

import Network.HTTP.Req

import Util.Text ( tshow )

-- | Interpreter for programs interacting with the Mimicle API only anonymously.
runMimicleAnon
    :: forall r a
    .  ( Members '[ Output Log
                  , Embed IO
                  ] r
       )
    => Url 'Https -> Sem (ApiAnon ': r) a -> Sem r a
runMimicleAnon base = interpret (runMimicleAnon' base)

runMimicleAnon'
    :: forall rInitial r a
    .  ( Members '[ Output Log
                  , Embed IO
                  ] r
       )
    => Url 'Https -> ApiAnon (Sem rInitial) a -> Sem r a
runMimicleAnon' base = \case

  AlbumLatest targetGender -> do
    let url = base /: "album"/:"latest"
        queryParams = "targetGender" =: targetGender
    reqPaginatedAll ("latest works ("<>tshow targetGender<>")") 100 defaultHttpConfig url queryParams

  Album' aid -> do
    let url = base /: "album"/:(getId aid)
    r <- requestIdWithLog "album" aid Nothing $
        runReq defaultHttpConfig $
            req GET url NoReqBody jsonResponse mempty
    pure $ responseBody r
