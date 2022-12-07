{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Mimidl.Mimicle.Api.Impl.MimicleCom.Common where

import Mimidl.Mimicle.Api.Impl.Log qualified as Mimicle

import Polysemy
import Polysemy.Error
import Polysemy.Output
import Data.WorldPeace

import Prettyprinter qualified as Pretty
import Prettyprinter.Render.String qualified as Pretty
import Prettyprinter ( Pretty(..) )

import System.Exit qualified
import Control.Monad.IO.Class
import Numeric.Natural
import Network.HTTP.Req
import Data.Aeson ( FromJSON )
import Mimidl.Mimicle.Types
import Data.Text ( Text )
import Util.Text ( tshow )

import Orphans() -- for Pretty OpenUnion

-- | Lift an error into an 'OpenUnion', and throw it inside Polysemy's 'Error'
--   effect.
throwOU
    :: forall es e r a
    .  (Member (Error (OpenUnion es)) r, IsMember e es)
    => e -> Sem r a
throwOU = throw . openUnionLift @e @es

--------------------------------------------------------------------------------

cleanup
    :: forall es r a
    .  ( r ~ '[ Error (OpenUnion es)
              , Embed IO, Final IO ]
       , Pretty (OpenUnion es) )
    => Sem r a -> IO a
cleanup =
      runFinal
    . embedToFinal @IO
    . catchPrintEnd
    . runError

-- | Catch, print, end.
catchPrintEnd
    :: (Pretty e, Member (Embed IO) r) => Sem r (Either e a) -> Sem r a
catchPrintEnd x = x >>= \case Right a -> pure a
                              Left  e -> liftIO $ do
                                            putStrLn $ prettyString e
                                            System.Exit.exitFailure

prettyString :: Pretty e => e -> String
prettyString =   Pretty.renderString
               . Pretty.layoutPretty Pretty.defaultLayoutOptions
               . pretty

--------------------------------------------------------------------------------

-- | @GET@ some paginated resource until you've gotten the whole lot.
--
-- Actions may (should) have maximum page sizes. 100 is a max page size for at
-- least one action. But it might differ for different actions and/or resources,
-- so be warned!
reqPaginatedAll
    :: (Members [Embed IO, Output Mimicle.Log] r, FromJSON a)
    => Text -> Natural -> HttpConfig -> Url 'Https -> Option 'Https
    -> Sem r [a]
reqPaginatedAll txt maxPageSize httpCfg url queryParams = do
    let range = "1-"<>tshow maxPageSize
    r <- Mimicle.requestWithLog txt (Just range) $ runReq httpCfg $
        req GET url NoReqBody jsonResponse (queryParams' (0 :: Natural) maxPageSize)
    let r' = responseBody r
        totalItems = paginatedTotal r'
    output $ Mimicle.LogRequestUpdate txt $ "total paginated items = "<>tshow totalItems
    let items = if paginatedTotal r' < paginatedTake r' then paginatedTotal r' else paginatedTake r'
    go (paginatedTotal r') [paginatedItems r'] (fromIntegral items)
  where
    queryParams' skip tak = "take" =: tak <> "skip" =: skip <> queryParams
    go totalItems items currentItems =
        let remainingItems = totalItems - currentItems
            pageSize = min remainingItems maxPageSize
        in  if pageSize == 0 then pure $ concat $ reverse items
            else do
                let range = tshow (currentItems+1)<>"-"<>tshow (currentItems+pageSize)
                r <- Mimicle.requestWithLog txt (Just range) $ runReq httpCfg $
                    req GET url NoReqBody jsonResponse (queryParams' currentItems pageSize)
                let r' = responseBody r
                    newItems = paginatedItems r'
                go totalItems (newItems : items) (pageSize + currentItems)
