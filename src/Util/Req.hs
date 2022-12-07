module Util.Req where

import Polysemy
import Network.HTTP.Req

embedReq :: Member (Embed IO) r => Req a -> Sem r a
embedReq = embed . runReq @IO defaultHttpConfig
