{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Download various metadata from Mimicle.

module Mimidl.Download.Metadata where

import Mimidl.Mimicle.Api.Model qualified as Mimicle
import Mimidl.Mimicle.Types qualified as Mimicle

import Mimidl.Mimicle.Api.Impl.MimicleCom qualified as Mimicle
import Mimidl.Mimicle.Api.Impl.Log qualified as Mimicle

import Network.HTTP.Req

runAllWorksByDate
    :: Mimicle.TargetGender -> Url 'Https -> IO [Mimicle.SearchAlbum]
runAllWorksByDate tg url = do
      Mimicle.cleanup @'[]
    . Mimicle.runLogIOConsole
    . Mimicle.runMimicleAnon url
    $ Mimicle.albumLatest tg

runWork
    :: Url 'Https -> Mimicle.Id -> IO Mimicle.Album
runWork url aid = do
      Mimicle.cleanup @'[]
    . Mimicle.runLogIOConsole
    . Mimicle.runMimicleAnon url
    $ Mimicle.album' aid
