{- TODO
  * These logs are designed to support an interpreter can log requests inside
    other requests by keeping a cheap state stack. But I am yet to write it.
-}

module Mimidl.Mimicle.Api.Impl.Log where

import Mimidl.Mimicle.Types ( Id )

import Polysemy
import Polysemy.Output

import Data.Text qualified as Text
import Data.Text ( Text )

import Control.Monad.IO.Class

-- | Combined type for logging Mimicle API actions.
--
-- Note that different parts of the Mimicle API may only log certain messages.
-- I tried using a compositional data type like for errors, but it complicated
-- things too much.
data Log
  = LogRequest -- ^ Log arbitrary artifact request.
        RequestStatus -- ^ are we logging request attempt, or success?
        Text -- ^ artifact type/name
        (Maybe Text) -- ^ extra info (e.g. pagination)

  | LogRequestId -- ^ Log arbitrary artifact request via ID.
        RequestStatus -- ^ are we logging request attempt, or success?
        Text -- ^ artifact type/name
        Id -- ^ artifact ID
        (Maybe Text) -- ^ extra info (e.g. pagination)

  | LogRequestUpdate
        Text -- ^ artifact type/name
        Text -- ^ update

data RequestStatus
  = AttemptingRequest
  | RequestSuccessful

-- | Bracket an action with a request log: log attempt, request, log success.
requestWithLog
    :: Members '[Output Log] r
    => Text -> Maybe Text -> Sem r a -> Sem r a
requestWithLog l mext p = do
    output (LogRequest AttemptingRequest l mext)
    a <- p
    output (LogRequest RequestSuccessful l mext)
    pure a

-- | Bracket an action with a ID request log: log attempt, request, log success.
requestIdWithLog
    :: Members '[Output Log] r
    => Text -> Id -> Maybe Text -> Sem r a -> Sem r a
requestIdWithLog l i mext p = do
    output (LogRequestId AttemptingRequest l i mext)
    a <- p
    output (LogRequestId RequestSuccessful l i mext)
    pure a

--------------------------------------------------------------------------------

-- | Write logs plainly to console.
runLogIOConsole :: Members '[Embed IO] r => Sem (Output Log ': r) a -> Sem r a
runLogIOConsole = interpret $ \case
  Output l -> liftIO $
    case l of
      LogRequest   reqst txt   txtextra -> do
        let extra = maybe mempty (\t -> " ("<>Text.unpack t<>")") txtextra
        putStrLn $ "Mimicle: "<>verb reqst<>Text.unpack txt<>extra
      LogRequestId reqst txt i txtextra -> do
        let extra = maybe mempty (\t -> " ("<>Text.unpack t<>")") txtextra
        putStrLn $ "Mimicle: "<>verb reqst<>Text.unpack txt<>": "<>show i<>extra
      LogRequestUpdate txt1 txt2 -> do
        putStrLn $ "Mimicle: "<>Text.unpack txt1<>": "<>Text.unpack txt2
  where
    verb :: RequestStatus -> String -- break in case of OverloadedStrings
    verb = \case
      AttemptingRequest -> "attempt: "
      RequestSuccessful -> "success: "
