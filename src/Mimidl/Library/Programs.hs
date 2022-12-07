module Mimidl.Library.Programs where

import Mimidl.Library.Monad qualified as Library
import Mimidl.Library.Monad ( Library )
import Mimidl.Mimicle.Api.Model qualified as Mimicle
import Mimidl.Mimicle.Types qualified as Mimicle

import Mimidl.Mimicle.Api.Impl.MimicleCom.Anon qualified as Mimicle
import Mimidl.Mimicle.Api.Impl.Log qualified as Mimicle

import Polysemy

import Control.Monad ( forM_ )
import Data.List ( nub )
import Path
import Network.HTTP.Req

downloadAllWorkMeta
    :: ( Members '[ Mimicle.ApiAnon
                  , Library
                  ] r
       )
    => Sem r ()
downloadAllWorkMeta = do
    sass <- mapM Mimicle.albumLatest allTargetGenders
    let aids = nubConcatSearchAlbumIds sass
    forM_ aids $ \aid -> Mimicle.album' aid >>= Library.writeWorkMeta

-- TODO
allTargetGenders :: [Mimicle.TargetGender]
allTargetGenders = [minBound .. maxBound]

nubConcatSearchAlbumIds :: [[Mimicle.SearchAlbum]] -> [Mimicle.Id]
nubConcatSearchAlbumIds = nub . concat . map (map Mimicle.searchAlbumId)

runDownloadAllWorkMeta :: Url 'Https -> Path Abs Dir -> IO ()
runDownloadAllWorkMeta url libDir = do
      runFinal
    . embedToFinal @IO
    . Mimicle.runLogIOConsole
    . Library.runLibraryFolderYaml libDir
    . Mimicle.runMimicleAnon url
    $ downloadAllWorkMeta
