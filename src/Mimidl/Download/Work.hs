-- | Download works owned by the authenticated account.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Mimidl.Download.Work where

import Mimidl.Mimicle.Types qualified as Mimicle
import Mimidl.Mimicle.Api.Model qualified as Mimicle

import Mimidl.Mimicle.Api.Impl.Log qualified as Mimicle
import Mimidl.Mimicle.Api.Impl.MimicleCom qualified as Mimicle

import Mimidl.Mimicle.Types.Json qualified as Mimicle

import HGCloud.Firebase qualified as GC.FB
import HGCloud.Firebase.Authentication qualified as GC.FB.Auth

import Polysemy
import Polysemy.Error
import Polysemy.State

import Data.WorldPeace

import Prettyprinter qualified as Pretty
import Prettyprinter ( Pretty(..) )

import GHC.Generics ( Generic )
import Control.Monad ( forM_ )
import Data.ByteString qualified as B
import Numeric.Natural ( Natural )
import Control.Monad.IO.Class
import Data.Text ( Text )
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import System.Directory
import Text.Printf
import Network.HTTP.Req
import Data.Aeson

data Env = Env
  { envApp  :: App
  , envNids :: [Mimicle.Nid]
  } deriving stock (Generic, Show, Eq)

data App = App
  { appApiBase    :: Url 'Https
  , appGCFBApiKey :: GC.FB.ApiKey
  } deriving stock (Generic, Show, Eq)
instance FromJSON App where
    parseJSON = Mimicle.gpjm "app"

data ENoSuchNidInPurchasedAlbums = ENoSuchNidInPurchasedAlbums Mimicle.Nid [Mimicle.Album]
    deriving stock (Generic, Show, Eq)
instance Pretty ENoSuchNidInPurchasedAlbums where pretty = Pretty.unsafeViaShow

program
    :: forall es r
    .  ( Members '[ Mimicle.Api
                  , Embed IO
                  , Error (OpenUnion es)
                  ] r
       , Contains '[ENoSuchNidInPurchasedAlbums] es )
    => Env -> Sem r ()
program env = do
    albums <- Mimicle.apiAuthd $ Mimicle.MyPurchasedAlbums
    forM_ (envNids env) $ \nid -> do
        case Mimicle.searchAlbumViaNid nid albums of
          Nothing -> Mimicle.throwOU @es $ ENoSuchNidInPurchasedAlbums nid albums
          Just a ->
            forM_ (zip [(1 :: Natural) ..] (Mimicle.albumTracks a)) $ \(n, track) -> do
                perm <- Mimicle.apiAuthd $ Mimicle.PermissionGenerate $ Mimicle.trackId track
                let trackDir = printf "%02d" n
                embed @IO $ do
                    createDirectory trackDir
                    withCurrentDirectory trackDir $ do
                        plBs   <- fDlStream perm "v1_h" "v1_h.m3u8"
                        B.writeFile "v1_h.m3u8" plBs
                        metaBs <- fDlStream perm "v1_h" "stream"
                        B.writeFile "stream"    metaBs
                        let tParts = fParseM3u8 plBs
                        forM_ [0..tParts] $ fDlPart perm "v1_h" -- TODO make concurrent!

run
    :: forall es
    .  ( es ~ '[ENoSuchNidInPurchasedAlbums, Mimicle.EAuthBackoff] )
    => Env -> Mimicle.AuthToken -> IO ()
run env atok =
      Mimicle.cleanup @es
    . Mimicle.runLogIOConsole
    . GC.FB.Auth.runApi (appGCFBApiKey (envApp env))
    . evalState @(Maybe GC.FB.Auth.IdToken) Nothing
    . Mimicle.runMimicle @es (appApiBase (envApp env)) atok
    $ program @es env

fDlStream :: MonadIO m => Mimicle.Permission -> Text -> Text -> m B.ByteString
fDlStream p folder name =
    Mimicle.downloadPermissionContent go p ["stream", folder, name]
  where
    go = \p' -> header "x-stream-token" (Text.encodeUtf8 (Mimicle.permissionToken p'))

-- TODO bad
fParseM3u8 :: B.ByteString -> Natural
fParseM3u8 = parse . last . filter notEmptyOrHash . Text.lines . Text.decodeUtf8
  where
    notEmptyOrHash t =
        case Text.uncons t of
          Nothing       -> False
          Just ('#', _) -> False
          _             -> True
    parse = tread . Text.take 5 . Text.drop 5

tread :: Read a => Text -> a
tread = read . Text.unpack

fDlPart :: MonadIO m => Mimicle.Permission -> Text -> Natural -> m ()
fDlPart p folder n = do
    let filename = folder<>"_"<>Text.pack (printf "%05d" n)<>".ts"
    bs <- fDlStream p folder filename
    liftIO $ B.writeFile (Text.unpack filename) bs

{- TODO next
extractAlbumTracks :: KeyMap Value -> Maybe (Vector (KeyMap Value))
KeyMap Value -> Maybe ()

* .tracks in album. For each one, get `id` and `file`. `id` goes to Google API
  call. `file` stores the various formats.
-}
